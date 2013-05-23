{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Data.Mat.Raw where

import Foreign
import Foreign.C

#include "matio.h"

#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

-- | A dummy tag for an unknown data pointer.
data CData

-- | A dummy tag for a MAT data file pointer.
data CMat

-- | A dummy tag for matio internal data.
data CMatvarInternal

data CMatComplexSplit a = CMatComplexSplit {
  re :: Ptr a,
  im :: Ptr a
}

instance Storable (CMatComplexSplit a) where
  sizeOf    _ = #{size      mat_complex_split_t}
  alignment _ = #{alignment mat_complex_split_t}

  peek ptr = do
    re <- #{peek mat_complex_split_t, Re} ptr
    im <- #{peek mat_complex_split_t, Im} ptr
    return $ CMatComplexSplit re im

  poke ptr v = do
    #{poke mat_complex_split_t, Re} ptr $ re v
    #{poke mat_complex_split_t, Im} ptr $ im v

-- | Raw representation of a variable. Nearly no marshalling being done here,
-- so that we can make 'MatVarT' into an instance of 'Storable' easily.
data CMatVar = CMatVar {
  nbytes      :: CSize,
  rank        :: CInt,
  dataType    :: MatioType,
  dataSize    :: CInt,
  classType   :: MatioClass,
  isComplex   :: Bool,
  isGlobal    :: Bool,
  isLogical   :: Bool,
  dims        :: Ptr CSize, -- actually an array
  name        :: CString,   -- can be null
  theData     :: Ptr CData, -- we explicitly do not dereference this pointer.
  memConserve :: Bool,
  compression :: MatioCompression,
  internal    :: Ptr CMatvarInternal
}

instance Storable CMatVar where
  sizeOf    _ = #{size      matvar_t}
  alignment _ = #{alignment matvar_t}

  peek ptr = do
    nbytes      <- #{peek matvar_t, nbytes      } ptr
    rank        <- #{peek matvar_t, rank        } ptr
    dataType    <- #{peek matvar_t, data_type   } ptr
    dataSize    <- #{peek matvar_t, data_size   } ptr
    classType   <- #{peek matvar_t, class_type  } ptr
    isComplex   <- #{peek matvar_t, isComplex   } ptr
    isGlobal    <- #{peek matvar_t, isGlobal    } ptr
    isLogical   <- #{peek matvar_t, isLogical   } ptr
    dims        <- #{peek matvar_t, dims        } ptr
    name        <- #{peek matvar_t, name        } ptr
    theData     <- #{peek matvar_t, data        } ptr
    memConserve <- #{peek matvar_t, mem_conserve} ptr
    compression <- #{peek matvar_t, compression } ptr
    internal    <- #{peek matvar_t, internal    } ptr
    return $ CMatVar nbytes rank dataType dataSize classType isComplex isGlobal isLogical dims name theData memConserve compression internal

  poke ptr v = do
    #{poke matvar_t, nbytes      } ptr $ nbytes      v
    #{poke matvar_t, rank        } ptr $ rank        v
    #{poke matvar_t, data_type   } ptr $ dataType    v
    #{poke matvar_t, data_size   } ptr $ dataSize    v
    #{poke matvar_t, class_type  } ptr $ classType   v
    #{poke matvar_t, isComplex   } ptr $ isComplex   v
    #{poke matvar_t, isGlobal    } ptr $ isGlobal    v
    #{poke matvar_t, isLogical   } ptr $ isLogical   v
    #{poke matvar_t, dims        } ptr $ dims        v
    #{poke matvar_t, name        } ptr $ name        v
    #{poke matvar_t, data        } ptr $ theData     v
    #{poke matvar_t, mem_conserve} ptr $ memConserve v
    #{poke matvar_t, compression } ptr $ compression v
    #{poke matvar_t, internal    } ptr $ internal    v

-- MAT enumeration types

newtype MatAcc = MatAcc CInt

#{enum MatAcc, MatAcc
  , rdonly = MAT_ACC_RDONLY
  , rdwr   = MAT_ACC_RDWR
}

newtype MatFT = MatFT CInt

#{enum MatFT, MatFT
  , mat73  = MAT_FT_MAT73
  , mat5   = MAT_FT_MAT5
  , mat4   = MAT_FT_MAT4
}

newtype MatioType = MatioType CInt
  deriving (Eq, Show, Storable)

#{enum MatioType, MatioType
  , tunknown    = MAT_T_UNKNOWN
  , tint8       = MAT_T_INT8
  , tuint8      = MAT_T_UINT8
  , tint16      = MAT_T_INT16
  , tuint16     = MAT_T_UINT16
  , tint32      = MAT_T_INT32
  , tuint32     = MAT_T_UINT32
  , tsingle     = MAT_T_SINGLE
  , tdouble     = MAT_T_DOUBLE
  , tint64      = MAT_T_INT64
  , tuint64     = MAT_T_UINT64
  , tmatrix     = MAT_T_MATRIX
  , tcompressed = MAT_T_COMPRESSED
  , tutf8       = MAT_T_UTF8
  , tutf16      = MAT_T_UTF16
  , tutf32      = MAT_T_UTF32
  , tstring     = MAT_T_STRING
  , tcell       = MAT_T_CELL
  , tstruct     = MAT_T_STRUCT
  , tarray      = MAT_T_ARRAY
  , tfunction   = MAT_T_FUNCTION
}

newtype MatioClass = MatioClass CInt
  deriving (Eq, Show, Storable)

#{enum MatioClass, MatioClass
  , cempty    = MAT_C_EMPTY
  , ccell     = MAT_C_CELL
  , cstruct   = MAT_C_STRUCT
  , cobject   = MAT_C_OBJECT
  , cchar     = MAT_C_CHAR
  , csparse   = MAT_C_SPARSE
  , cdouble   = MAT_C_DOUBLE
  , csingle   = MAT_C_SINGLE
  , cint8     = MAT_C_INT8
  , cuint8    = MAT_C_UINT8
  , cint16    = MAT_C_INT16
  , cuint16   = MAT_C_UINT16
  , cint32    = MAT_C_INT32
  , cuint32   = MAT_C_UINT32
  , cint64    = MAT_C_INT64
  , cuint64   = MAT_C_UINT64
  , cfunction = MAT_C_FUNCTION
}

newtype MatioCompression = MatioCompression CInt
  deriving (Eq, Show, Storable)

#{enum MatioCompression, MatioCompression
  , noCompression   = MAT_COMPRESSION_NONE
  , zlibCompression = MAT_COMPRESSION_ZLIB
}

-- Raw MAT file functions

foreign import ccall unsafe "matio.h Mat_CreateVer"       cMatCreateVer       :: CString -> CString -> MatFT -> IO (Ptr CMat)
foreign import ccall unsafe "matio.h Mat_Close"           cMatClose           :: Ptr CMat -> IO Int
foreign import ccall unsafe "matio.h Mat_Open"            cMatOpen            :: CString -> MatAcc -> IO (Ptr CMat)
foreign import ccall unsafe "matio.h Mat_GetFilename"     cMatGetFilename     :: Ptr CMat -> IO CString
foreign import ccall unsafe "matio.h Mat_GetVersion"      cMatGetVersion      :: Ptr CMat -> IO MatFT
foreign import ccall unsafe "matio.h Mat_Rewind"          cMatRewind          :: Ptr CMat -> IO CInt

-- Raw MAT variable access functions

foreign import ccall unsafe "matio.h Mat_VarReadInfo"     cMatVarReadInfo     :: Ptr CMat -> CString -> IO (Ptr CMatVar)
foreign import ccall unsafe "matio.h Mat_VarRead"         cMatVarRead         :: Ptr CMat -> CString -> IO (Ptr CMatVar)
foreign import ccall unsafe "matio.h Mat_VarReadNextInfo" cMatVarReadNextInfo :: Ptr CMat -> IO (Ptr CMatVar)
foreign import ccall unsafe "matio.h Mat_VarReadNext"     cMatVarReadNext     :: Ptr CMat -> IO (Ptr CMatVar)
foreign import ccall unsafe "matio.h Mat_VarFree"         cMatVarFree         :: Ptr CMatVar -> IO ()

-- Raw MAT functions for creating and writing variables

foreign import ccall unsafe "matio.h Mat_VarCreate"       cMatVarCreate       :: CString
                                                                              -> MatioClass
                                                                              -> MatioType
                                                                              -> CInt
                                                                              -> Ptr CSize
                                                                              -> Ptr CData
                                                                              -> CInt
                                                                              -> IO (Ptr CMatVar)

-- Raw MAT functions for dealing with structs

foreign import ccall unsafe "matio.h Mat_VarGetNumberOfFields" cMatVarGetNumberOfFields
                                                                              :: Ptr CMatVar -> IO CUInt
foreign import ccall unsafe "matio.h Mat_VarGetStructFieldnames" cMatVarGetStructFieldnames
                                                                              :: Ptr CMatVar -> IO (Ptr CString)

-- Raw MAT debugging functions

foreign import ccall unsafe "matio.h Mat_VarPrint"        cMatVarPrint        :: Ptr CMatVar -> CInt -> IO ()
