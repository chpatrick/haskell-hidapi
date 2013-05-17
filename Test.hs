-- Open and dump the first HID device. Should print deltas (?) for a USB mouse.
-- You need root or a udev rule for your device.
-- https://github.com/signal11/hidapi/blob/master/udev/99-hid.rules
-- This seems to claim the device until it is unplugged.

import Control.Monad.Error
import Control.Monad.Trans
import Data.Int
import qualified Data.ByteString as BS
import System.HIDAPI as HID

main = withHIDAPI $ do
  ( di : dis ) <- enumerateAll
  d <- openDeviceInfo di
  forever $ do
    bs <- HID.read d 6
    putStr (show (fromIntegral (BS.index bs 1) :: Int8))
    putChar ' '
    print (fromIntegral (BS.index bs 2) :: Int8)
