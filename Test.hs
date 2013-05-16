-- Open and dump the first HID device. Should print deltas (?) for a USB mouse.
-- You need root or a udev rule for your device.
-- https://github.com/signal11/hidapi/blob/master/udev/99-hid.rules
-- This seems to claim the device until it is unplugged.

import Control.Monad.Error
import Control.Monad.Trans
import qualified Data.ByteString as BS
import System.HIDAPI as HID

main = runErrorT $ do
	ErrorT HID.init
	( di : dis ) <- ErrorT enumerateAll
	d <- ErrorT $ openDeviceInfo di
	forever $ do
		bs <- ErrorT $ HID.read d 6
		liftIO $ do
			putStr (show (BS.index bs 1))
			putChar ' '
			print (BS.index bs 2)
