{-# LANGUAGE GADTs, FlexibleContexts #-}

module Main(main) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game


import System.Environment
import Data.ByteString (pack, unpack, ByteString, readFile)
import qualified Data.Vector.Storable.Mutable as V
import Data.Vector.Storable (toList, create, freeze)
--import Data.Vector
import qualified Data.List.Safe as SAFE
import Data.Word
import Data.Bits
import Data.IORef
import Control.Monad

data Machine = Machine {  ram :: V.IOVector Word8
                        , regs :: V.IOVector Word8
                        , ap :: IORef Word16
                        , sp :: IORef Word16
                        , pc :: IORef Word16
                        , time :: IORef Word8
                        , delay :: IORef Word8
                        , frameBuffer :: V.IOVector Bool
                        }

newMachine :: IO Machine
newMachine = do
    ap <- newIORef 0x0
    sp <- newIORef 0x0
    pc <- newIORef 0x200
    time <- newIORef 0x0
    delay <- newIORef 0x0
    ram <- V.replicate 0x1000 0x0
    regs <- V.replicate 0x10 0x0
    frameBuffer <- V.replicate (64 * 32) False
    pure $ Machine ram regs ap sp pc time delay frameBuffer

writeRam :: Machine -> Word16 -> Word8 -> IO ()
writeRam machine addr = V.write (ram machine) (fromIntegral addr)

writeW16 machine addr w16 = writeRam machine addr (fromIntegral $ shiftR w16 8) >>
                            writeRam machine (addr+1) (fromIntegral $ w16 .&. 0xFF) 

readInstruction :: Machine -> Word16 -> IO Word16
readInstruction machine addr = do let read = fromIntegral . readRam machine
                                  firstByte <- read addr
                                  secondByte <- read (addr+1)
                                  return $ shiftL firstByte 8 .|. secondByte


readRam :: Machine -> Word16 -> IO Word8
readRam machine addr = V.read (ram machine) (fromIntegral addr)

loadProgram :: Machine -> ByteString -> IO Machine
loadProgram machine bs = do zipWithM_ (writeRam machine) addressSpace byteList 
                            return machine
    where byteList = unpack bs
          addressSpace = [0x200..0x200 + fromIntegral (length byteList)]

type Reg = Int

--data Commands = FX0A Reg

window :: Display
window = InWindow "Chip-8" (64*scale, 32*scale) (10, 10)
        where scale = 10

background :: Color
background = black

drawing :: Machine -> IO Picture
drawing machine = do
    imageVector <- freeze $ frameBuffer machine
    let imageData = pack $ mconcat $ map palette (toList imageVector)
    return $ bitmapOfByteString 64 32 format imageData False 
                where format = BitmapFormat TopToBottom PxRGBA
                      palette True = [0x00,0xFF,0x00,0xFF]
                      palette False = [0xFF,0x00,0x00,0xFF]

eventHandler :: Event -> Machine -> IO Machine
eventHandler (EventKey (Char c) _ _ _) = simpleEventHandler c
eventHandler _ = return

simpleEventHandler '1' = return

decr = flip modifyIORef (subtract 1)
incr = flip modifyIORef (+1)

tick :: Machine -> IO Machine
tick machine = fetch >>= execute machine >> modifyIORef (pc machine) (+2)
        where fetch = readIORef (pc machine) >>= readInstruction machine

execute machine@(Machine ram regs ap sp pc time delay frameBuffer) word16 = ex (get4Bit 3) (get4Bit 2) (get4bit 1) (get4bit 0)
    where get4Bit i = shiftR (word16 .&. shiftL 0xF (4*i)) (4*i)
          bit4ToByte a b = shiftL a 4 .|. b
          bit4To12Bit a b c = shiftL c 8 .|. bit4ToByte a b
          comp f x a b
            | V.read regs (fromIntegral x) `f` fromIntegral (bit4ToByte a b) = modifyIORef pc (+2)
            | otherwise = return ()
          ex 0x0 0x0 0xE 0x0 = undefined
          ex 0x0 0x0 0xE 0xE = readIORef sp >>= readInstruction machine >>= writeIORef pc >> modifyIORef sp (+2)
          ex 0x1 a b c = writeIORef pc (bit4To12Bit a b c)
          ex 0x2 a b c = do incr sp
                            sp2 <- readIORef sp
                            pc2 <- readIORef pc
                            writeW16 machine pc2 sp2
                            writeIORef pc (bit4To12Bit a b c)
          ex 0x3 = comp (==)        
          ex 0x4 = comp (/=)
          ex 0x5 x y 0x0
                    | V.read regs x == V.read regs y = modifyIORef (pc machine) (+2)
                    | otherwise = return ()
          ex 0x6 x a b = V.write regs x (bit4ToByte a b)
          ex 0x7 x a b = do regx <- V.read regs x
                            V.write regs x (regx + bit4ToByte a b)
          ex 0x8 x y 0x0 = V.read regs y >>= V.write regs x
          ex 0x8 x y 0x0 =
          ex 0x0 _ _ _ = error "SYS addr instruction unimplemented"

apply :: (Monad m) => Int -> (a -> m a) -> a -> m a
apply n step world = foldl (>>=) (return world) (replicate n step)

update t machine = do decr (time machine)
                      replicateM_ 9 (tick machine)

main :: IO ()
main = do mach <- newMachine
          binary <- (head <$> getArgs) >>= Data.ByteString.readFile
          bootedMachine <- loadProgram mach binary
          --freeze (ram bootedMachine) >>= print
          playIO window background 60 bootedMachine drawing eventHandler update