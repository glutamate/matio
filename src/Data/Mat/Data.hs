{-# LANGUAGE GADTs, StandaloneDeriving, KindSignatures, FlexibleContexts, UndecidableInstances, FlexibleInstances #-}
module Data.Mat.Data where

import Control.Monad
import Control.Monad.Trans
import Data.Complex
import Data.Functor
import Data.List
import Foreign
import Foreign.C

import Data.Mat.MemParser
import Data.Mat.Raw

-- | Representation of a MAT data type.
--
-- The first argument determines whether this is a type pattern or an
-- actual type.
--
-- The second argument is the "underlying" datatype.
--
-- The third argument is the Haskell result type.
--
-- TODO: Extend the documentation.
data MatType :: (* -> *) -> * -> * -> * where
  -- Base types
  MatDouble  :: MatType d Double Double  -- TODO: should this be CDouble?
  MatInt64   :: MatType d Int64  Int64   -- TODO: should this be CInt64?
  MatBool    :: MatType d Word8  Bool    -- TODO: should this be CUInt8?
  MatChar    :: MatType d CUChar Char

  -- Dynamic type
  MatCell    :: MatType d (Ptr CMatVar) MatData
    -- ^ Represents a single cell, i.e., a single value of arbitrary type.

  -- Compound types
  MatComplex :: MatType d a b -> MatType d (CMatComplexSplit a) (Complex b)
  MatStruct  :: d [String] -> MatType d (Ptr CMatVar) MatStruct
    -- ^ Represents a struct array with the given number of fields.
    -- TODO: The representation should contain the names of the fields.

  MatArray   :: MatType d a b -> d Int -> MatType d a [b]
    -- ^ Represents an array with the given element type and number of
    -- elements.

  -- Other
  MatUnknown :: MatioClass -> MatioType -> MatType d (Ptr CData) ()
    -- ^ Represents a value we do not know how to interpret properly as
    -- a Haskell type. We store the class and type of the value.

showMatType :: MatType Known a b -> String
showMatType MatDouble              = "Double"
showMatType MatInt64               = "Int64"
showMatType MatBool                = "Bool"
showMatType MatChar                = "Char"
showMatType MatCell                = "Cell"
showMatType (MatComplex t)         = "Complex(" ++ showMatType t ++ ")"
showMatType (MatStruct (Known fs)) = "Struct(" ++ show (intercalate ", " fs) ++ ")"
showMatType (MatArray t (Known d)) = "[" ++ showMatType t ++ "]_" ++ show d
showMatType (MatUnknown c t)       = "Unknown(Class " ++ show c ++ ", Type " ++ show t ++ ")"

-- | Contains a dictionary for the 'Show' class.
data ShowDict a where
  ShowDict :: (Show a) => ShowDict a

-- | For a given MAT type description, compute a corresponding 'Show'
-- dictionary.
matShowDict :: MatType d a b -> ShowDict b
matShowDict MatDouble        = ShowDict
matShowDict MatInt64         = ShowDict
matShowDict MatBool          = ShowDict
matShowDict MatChar          = ShowDict
matShowDict MatCell          = ShowDict
matShowDict (MatComplex t)   = case matShowDict t of
  ShowDict -> ShowDict
matShowDict (MatStruct _)    = ShowDict
matShowDict (MatArray t _)   = case matShowDict t of
  ShowDict -> ShowDict
matShowDict (MatUnknown _ _) = ShowDict

-- | This type can be used in 'MatType' in order to represent known
-- dimensions.
newtype Known a = Known a
  deriving (Show)

-- | This type can be used in 'MatType' in order to represent unknown
-- dimensions.
data Unknown a = Unknown
  deriving (Show)

-- deriving instance (Show (d Int), Show (d [String])) => Show (MatType d a b)
-- deriving instance Eq   d => Eq   (MatType d a b)
-- deriving instance Ord  d => Ord  (MatType d a b)
instance Show (MatType Known a b) where
  show t = showMatType t

-- | Type synonym for a struct.
type MatStruct = [MatData]
  -- TODO: would [(String, MatData)] or Map String MatData be better?
  -- The type already contains the field names, but perhaps a little
  -- redundancy is ok and pragmatic here?

-- | An arbitrary 'MatType' in a wrapper.
data AnyMatType :: * where
  AnyMatType :: (Storable a) => MatType Known a b -> AnyMatType

deriving instance Show (AnyMatType)
-- deriving instance Eq   (AnyMatType)
-- deriving instance Ord  (AnyMatType)

-- | Some piece of data paired with a type annotation.
data MatData :: * where
  MatData :: MatType Known a b -> b -> MatData

-- deriving instance Show (MatData)
instance Show MatData where
  show (MatData t d) = case matShowDict t of
    ShowDict -> show d ++ " :: " ++ show t

-- TODO: Think about a proper 'Show' instance.

-- | Given a type representation, 'matParser' computes a memory parser
-- for that type.
matParser :: MatType Known a b -> MemParser a b
matParser MatDouble              = storable
matParser MatInt64               = storable
matParser MatBool                = toBool <$> storable
matParser MatChar                = castCUCharToChar <$> storable
matParser (MatComplex t)         = complex (matParser t)
matParser (MatStruct (Known d))  = do
  ptrs <- array (length d) storable
  liftIO $ forM ptrs $ matDataPtr
matParser MatCell                = do
  ptr <- storable
  liftIO (matDataPtr ptr)
matParser (MatArray t (Known n)) = array n (matParser t)
matParser (MatUnknown _ _)       = nothing

-- | Given the raw variable data structure, get the name
-- of the variable.
matVarName :: CMatVar -> IO (Maybe String)
matVarName v = maybePeek peekCString (name v)

-- | Given the raw variable data structure, get the dimensions
-- of the variable.
--
-- TODO: Can the dimensions pointer be null?
matVarDims :: CMatVar -> IO [Int]
matVarDims v = map fromIntegral <$> peekArray (fromIntegral $ rank v) (dims v)

-- | Compute the type of the represented data.
matType :: Ptr CMatVar -> CMatVar -> IO AnyMatType
matType ptr v = do
  base <- matBaseType
  ds   <- matVarDims v
  return (matComplex (matDims ds base))
  where
    matBaseType :: IO AnyMatType
    matBaseType
      | dataType v == tdouble = return $ AnyMatType MatDouble
      | dataType v == tint64  = return $ AnyMatType MatInt64
      | dataType v == tuint8 && classType v == cchar = return $ AnyMatType MatChar
      | dataType v == tuint8 && isLogical v          = return $ AnyMatType MatBool 
      | dataType v == tstruct = do
          nrfields <- cMatVarGetNumberOfFields ptr
          fields   <- cMatVarGetStructFieldnames >=> peekArray (fromIntegral nrfields) >=> mapM peekCString $ ptr
          return $ AnyMatType (MatStruct (Known fields))
      | dataType v == tcell   = return $ AnyMatType MatCell
      | otherwise             = return $ AnyMatType $ MatUnknown (classType v) (dataType v)

    matDims :: [Int] -> AnyMatType -> AnyMatType
    matDims []       t = t
    matDims (d : ds) t =
      case matDims ds t of
        AnyMatType r -> AnyMatType (MatArray r (Known (fromIntegral d)))

    matComplex :: AnyMatType -> AnyMatType
    matComplex t | isComplex v =
      case t of
        AnyMatType r -> AnyMatType (MatComplex r)
                 | otherwise   = t

matTypePtr :: Ptr CMatVar -> IO AnyMatType
matTypePtr ptr = peek ptr >>= matType ptr

-- | Given a pointer to a MAT variable, provide the data in a suitable
-- Haskell structure, wrapped up with a type representation.
matData :: Ptr CMatVar -> CMatVar -> IO MatData
matData ptr v = do
  AnyMatType t <- matType ptr v
  let mp = matParser t
  x <- runMemParser mp (castPtr (theData v))
  return (MatData t x)

matDataPtr :: Ptr CMatVar -> IO MatData
matDataPtr ptr = peek ptr >>= matData ptr

data Equal :: * -> * -> * where
  Refl :: Equal a a

matTypeEqual :: MatType d a b -> MatType d' a' c -> Maybe (Equal b c)
matTypeEqual MatDouble  MatDouble = return Refl
matTypeEqual MatInt64   MatInt64  = return Refl
matTypeEqual MatBool    MatBool   = return Refl
matTypeEqual MatChar    MatChar   = return Refl
matTypeEqual MatCell    MatCell   = return Refl
matTypeEqual (MatComplex s) (MatComplex t) = do
  Refl <- matTypeEqual s t
  return Refl
matTypeEqual (MatStruct _) (MatStruct _) = return Refl
matTypeEqual (MatArray s _) (MatArray t _) = do
  Refl <- matTypeEqual s t
  return Refl
matTypeEqual _ _ = Nothing

-- | Given a value of unknown type and a type representation,
-- attempt a cast.
matCast :: MatData -> MatType d a b -> Maybe b
matCast (MatData s x) t =
  case matTypeEqual s t of
    Just Refl -> Just x
    Nothing   -> Nothing

dim2 :: MatType Unknown a b -> MatType Unknown a [[b]]
dim2 t = MatArray (MatArray t Unknown) Unknown
