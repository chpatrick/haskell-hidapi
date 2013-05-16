{-# LANGUAGE ForeignFunctionInterface #-}

#include "hidapi/hidapi.h"

module System.HIDAPI
  ( System.HIDAPI.init
  , exit
  , enumerate, enumerateAll
  , open, openPath, openDeviceInfo
  , close
  , System.HIDAPI.read
  , getSerialNumberString
  , System.HIDAPI.error
  ) where

import Control.Applicative
import Data.ByteString
import Data.Maybe
import Foreign
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal
import Foreign.Ptr
import Prelude

newtype Device = Device (Ptr ())

data DeviceInfoInternal = DeviceInfoInternal
  { _path :: CString
  , _vendorId :: CUShort
  , _productId :: CUShort
  , _serialNumber :: CWString
  , _releaseNumber :: CUShort
  , _manufacturerString :: CWString
  , _productString :: CWString
  , _usagePage :: CUShort
  , _usage :: CUShort
  , _interfaceNumber :: CInt
  , _next :: Ptr DeviceInfoInternal
  } deriving (Show)

#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

instance Storable DeviceInfoInternal where
  alignment _ = #{alignment struct hid_device_info}
  sizeOf _ = #{size struct hid_device_info}
  peek p = DeviceInfoInternal <$>
    #{peek struct hid_device_info, path} p <*>
    #{peek struct hid_device_info, vendor_id} p <*>
    #{peek struct hid_device_info, product_id} p <*>
    #{peek struct hid_device_info, serial_number} p <*>
    #{peek struct hid_device_info, release_number} p <*>
    #{peek struct hid_device_info, manufacturer_string} p <*>
    #{peek struct hid_device_info, product_string} p <*>
    #{peek struct hid_device_info, usage_page} p <*>
    #{peek struct hid_device_info, usage} p <*>
    #{peek struct hid_device_info, interface_number} p <*>
    #{peek struct hid_device_info, next} p

data DeviceInfo = DeviceInfo
  { path :: String
  , vendorId :: Word16
  , productId :: Word16
  , serialNumber :: Maybe String
  , releaseNumber :: Word16
  , manufacturerString :: Maybe String
  , productString :: Maybe String
  , usagePage :: Word16
  , usage :: Word16
  , interfaceNumber :: Int
  } deriving (Show)

peekOptString p
  | p == nullPtr = return Nothing
  | otherwise = Just <$> peekCWString p

fromInternalDeviceInfo :: DeviceInfoInternal -> IO DeviceInfo
fromInternalDeviceInfo di = DeviceInfo <$>
  peekCString (_path di) <*>
  pure (fromIntegral $ _vendorId di) <*>
  pure (fromIntegral $ _productId di) <*>
  peekOptString (_serialNumber di) <*>
  pure (fromIntegral $ _releaseNumber di) <*>
  peekOptString (_manufacturerString di) <*>
  peekOptString (_productString di) <*>
  pure (fromIntegral $ _usagePage di) <*>
  pure (fromIntegral $ _usage di) <*>
  pure (fromIntegral $ _interfaceNumber di)

foreign import ccall unsafe "hidapi/hidapi.h hid_error"
  hid_error :: IO CWString

type IOFallible a = IO (Either String a)

error :: IO (Maybe String)
error = do
  e <- hid_error
  if e == nullPtr
    then return Nothing
    else do
      es <- peekCWString e
      free e
      return (Just es)

handleError :: Bool -> IO a -> IO (Either String a)
handleError True f = Left . fromMaybe "Unknown error" <$> System.HIDAPI.error
handleError False f  = Right <$> f

foreign import ccall unsafe "hidapi/hidapi.h hid_init"
  hid_init :: IO CInt

init :: IOFallible ()
init = do
  r <- hid_init
  handleError (r /= 0) (return ())

foreign import ccall unsafe "hidapi/hidapi.h hid_exit"
  hid_exit :: IO CInt

exit :: IOFallible ()
exit = do
	r <- hid_exit
	handleError (r /= 0) (return ())

foreign import ccall unsafe "hidapi/hidapi.h hid_enumerate"
  hid_enumerate :: CUShort -> CUShort -> IO (Ptr DeviceInfoInternal)

foreign import ccall unsafe "hidapi/hidapi.h hid_free_enumeration"
  hid_free_enumeration :: Ptr DeviceInfoInternal -> IO ()

parseEnumeration :: Ptr DeviceInfoInternal -> IO [ DeviceInfo ]
parseEnumeration dip
  | dip == nullPtr = return []
  | otherwise = do
    idi <- peek dip
    di <- fromInternalDeviceInfo idi
    dis <- parseEnumeration (_next idi)
    return (di : dis)

enumerate :: Maybe Word16 -> Maybe Word16 -> IOFallible [ DeviceInfo ]
enumerate vendorId productId = do
  dip <- hid_enumerate (maybe 0 fromIntegral vendorId) (maybe 0 fromIntegral productId)
  if dip == nullPtr -- could be error or empty list
    then do
      e <- System.HIDAPI.error
      return $ case e of
        Nothing -> Right []
        Just err -> Left err
    else do
      dis <- parseEnumeration dip
      hid_free_enumeration dip
      return (Right dis)

enumerateAll :: IOFallible [ DeviceInfo ]
enumerateAll = enumerate Nothing Nothing

foreign import ccall unsafe "hidapi/hidapi.h hid_open"
  hid_open :: CUShort -> CUShort -> CWString -> IO (Ptr ())

open :: Word16 -> Word16 -> Maybe String -> IOFallible Device
open vendorId productId serialNumber = do
  let vid = fromIntegral vendorId
  let pid = fromIntegral productId
  dp <- case serialNumber of
    Nothing -> hid_open vid pid nullPtr
    Just sn -> withCWString sn (hid_open vid pid)
  handleError (dp == nullPtr) (return (Device dp))

foreign import ccall unsafe "hidapi/hidapi.h hid_open_path"
  hid_open_path :: CString -> IO (Ptr ())

openPath :: String -> IOFallible Device
openPath p = do
  dp <- withCString p hid_open_path
  handleError (dp == nullPtr) (return (Device dp))

openDeviceInfo :: DeviceInfo -> IOFallible Device
openDeviceInfo
  = openPath . path

foreign import ccall unsafe "hidapi/hidapi.h hid_close"
  close :: Device -> IO ()

foreign import ccall unsafe "hidapi/hidapi.h hid_read"
  hid_read :: Device -> Ptr CChar -> CSize -> IO CInt

read :: Device -> Int -> IOFallible ByteString
read d n = allocaBytes n $ \b -> do
  n' <- hid_read d b (fromIntegral n)
  handleError (n' == -1) (packCStringLen ( b, fromIntegral n' ))

foreign import ccall unsafe "hidapi/hidapi.h hid_get_serial_number_string"
  hid_get_serial_number_string :: Device -> CWString -> CSize -> IO CInt

_SERIAL_NUMBER_MAX_LENGTH = 32768

withMallocBytes :: Int -> (Ptr a -> IO b) -> IO b
withMallocBytes n f = do
  b <- mallocBytes n
  r <- f b
  free b
  return r

getSerialNumberString :: Device -> IOFallible String
getSerialNumberString d =
  withMallocBytes (_SERIAL_NUMBER_MAX_LENGTH * sizeOf (undefined :: CWchar)) $ \b -> do
    n' <- hid_get_serial_number_string d b (fromIntegral _SERIAL_NUMBER_MAX_LENGTH)
    handleError (n' == -1) (peekCWString b)