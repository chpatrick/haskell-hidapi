{-# LANGUAGE ForeignFunctionInterface #-}

#include "hidapi/hidapi.h"

module System.HIDAPI(System.HIDAPI.init) where

import Control.Applicative
import Foreign
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr

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

foreign import ccall unsafe "hidapi/hidapi.h hid_init"
  hid_init :: IO CInt

init :: IO Bool
init = (==0) <$> hid_init

foreign import ccall unsafe "hidapi/hidapi.h hid_enumerate"
  hid_enumerate :: CUShort -> CUShort -> IO (Ptr DeviceInfoInternal)

foreign import ccall unsafe "hidapi/hidapi.h hid_free_enumeration"
  hid_free_enumeration :: Ptr DeviceInfoInternal -> IO ()

enumerate :: Maybe Word16 -> Maybe Word16 -> IO [ DeviceInfo ]
enumerate vendorId productId = do
	dip <- hid_enumerate (maybe 0 fromIntegral vendorId) (maybe 0 fromIntegral productId)
	let parse dip = do
		idi <- peek dip
		di <- fromInternalDeviceInfo idi
		dis <- if _next idi == nullPtr
			then return []
			else parse (_next idi)
		return (di : dis)
	dis <- parse dip
	hid_free_enumeration dip
	return dis