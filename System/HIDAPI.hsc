{-# LANGUAGE ForeignFunctionInterface, DeriveDataTypeable #-}

#include "hidapi/hidapi.h"

module System.HIDAPI
  ( System.HIDAPI.init, exit, withHIDAPI
  , enumerate, enumerateAll
  , open, openPath, openDeviceInfo
  , close
  , System.HIDAPI.read
  , getSerialNumberString
  , System.HIDAPI.error
  , HIDAPIException(HIDAPIException)
  , Device()
  , DeviceInfo (..)
  , DevicePath
  , VendorID
  , ProductID
  , ReleaseNumber
  , SerialNumber
  , InterfaceNumber
  ) where

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.ByteString
import Data.Maybe
import Data.Typeable
import Data.Data
import Foreign
import Foreign.C.Types
import Foreign.C.String

type Hid_Device_Ptr = Ptr () -- used where hid_device* is used on C side
newtype Device = Device Hid_Device_Ptr

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

-- Aliases for writing better type signatures and haddocks.
type DevicePath = String
type VendorID = Word16
type ProductID = Word16
type ReleaseNumber = Word16
type SerialNumber = String
type InterfaceNumber = Int

data DeviceInfo = DeviceInfo
  { path :: DevicePath
  , vendorId :: VendorID
  , productId :: ProductID
  , serialNumber :: Maybe SerialNumber
  , releaseNumber :: ReleaseNumber
  , manufacturerString :: Maybe String
  , productString :: Maybe String
  , usagePage :: Word16
  , usage :: Word16
  , interfaceNumber :: InterfaceNumber
  } deriving (Show)

peekOptString :: Ptr CWchar -> IO (Maybe String)
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

data HIDAPIException = HIDAPIException String String
  deriving (Data, Typeable)

instance Show HIDAPIException where
  showsPrec _ (HIDAPIException a c) = showString a . showString ": " . showString c

instance Exception HIDAPIException

error :: IO (Maybe String)
error = do
  e <- hid_error
  if e == nullPtr
    then return Nothing
    else do
      es <- peekCWString e
      free e
      return (Just es)

check :: Bool -> String -> IO ()
check c m = unless c $ do
  e <- fromMaybe "Unknown error" <$> System.HIDAPI.error
  throwIO $ HIDAPIException m e

foreign import ccall unsafe "hidapi/hidapi.h hid_init"
  hid_init :: IO CInt

init :: IO ()
init = do
  r <- hid_init
  check (r == 0) "HIDAPI initialization failed"

foreign import ccall unsafe "hidapi/hidapi.h hid_exit"
  hid_exit :: IO CInt

exit :: IO ()
exit = do
	r <- hid_exit
	check (r == 0) "HIDAPI shutdown failed"

withHIDAPI :: IO a -> IO a
withHIDAPI = bracket_ System.HIDAPI.init exit

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

enumerate :: Maybe VendorID -> Maybe ProductID -> IO [ DeviceInfo ]
enumerate m'vendorId m'productId = do
  dip <- hid_enumerate (maybe 0 fromIntegral m'vendorId) (maybe 0 fromIntegral m'productId)
  if dip == nullPtr -- could be error or empty list
    then do
      e <- System.HIDAPI.error
      case e of
        Nothing -> return []
        Just err -> throwIO (HIDAPIException "Device enumeration failed" err)
    else do
      dis <- parseEnumeration dip
      hid_free_enumeration dip
      return dis

enumerateAll :: IO [ DeviceInfo ]
enumerateAll = enumerate Nothing Nothing

foreign import ccall unsafe "hidapi/hidapi.h hid_open"
  hid_open :: CUShort -> CUShort -> CWString -> IO Hid_Device_Ptr

open :: VendorID -> ProductID -> Maybe SerialNumber -> IO Device
open vendor_id product_id serial = do
  let vid = fromIntegral vendor_id
  let pid = fromIntegral product_id
  dp <- case serial of
    Nothing -> hid_open vid pid nullPtr
    Just sn -> withCWString sn (hid_open vid pid)
  check (dp /= nullPtr) "Device open (by vendor/product id) failed"
  return (Device dp)

foreign import ccall unsafe "hidapi/hidapi.h hid_open_path"
  hid_open_path :: CString -> IO Hid_Device_Ptr

openPath :: DevicePath -> IO Device
openPath p = do
  dp <- withCString p hid_open_path
  check (dp /= nullPtr) "Device open (by path) failed"
  return (Device dp)

openDeviceInfo :: DeviceInfo -> IO Device
openDeviceInfo
  = openPath . path

foreign import ccall unsafe "hidapi/hidapi.h hid_close"
  close :: Device -> IO ()

foreign import ccall unsafe "hidapi/hidapi.h hid_read"
  hid_read :: Device -> Ptr CChar -> CSize -> IO CInt

read :: Device -> Int -> IO ByteString
read d n = allocaBytes n $ \b -> do
  n' <- hid_read d b (fromIntegral n)
  check (n' /= -1) "Read failed"
  packCStringLen ( b, fromIntegral n' )

foreign import ccall unsafe "hidapi/hidapi.h hid_get_serial_number_string"
  hid_get_serial_number_string :: Device -> CWString -> CSize -> IO CInt

_SERIAL_NUMBER_MAX_LENGTH :: Int
_SERIAL_NUMBER_MAX_LENGTH = 32768

getSerialNumberString :: Device -> IO SerialNumber
getSerialNumberString d = do
  let bs = _SERIAL_NUMBER_MAX_LENGTH * sizeOf (undefined :: CWchar)
  bracket (mallocBytes bs) free $ \b -> do
    n' <- hid_get_serial_number_string d b (fromIntegral _SERIAL_NUMBER_MAX_LENGTH)
    check (n' /= -1) "Getting serial number failed"
    peekCWString b
