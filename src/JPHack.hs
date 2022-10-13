{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module JPHack
    ( 
      savePngImageWithoutCompression
    ) where

import Codec.Picture
import Data.Binary( encode )
import Data.Word(Word8)
import qualified Codec.Compression.Zlib as Z
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Lazy as Lb
import qualified Data.Vector.Storable as VS
import qualified Data.ByteString.Lazy.Char8 as LbU

import Codec.Picture.Png.Internal.Type
import Codec.Picture.Png.Internal.Metadata
import Codec.Picture.Metadata( Metadatas )

import Foreign.Storable( Storable, sizeOf )

#if !MIN_VERSION_base(4,8,0)
import Foreign.ForeignPtr.Safe( ForeignPtr, castForeignPtr )
#else
import Foreign.ForeignPtr( ForeignPtr, castForeignPtr )
#endif

savePngImageWithoutCompression :: FilePath -> DynamicImage -> IO ()
savePngImageWithoutCompression path img = Lb.writeFile path $ imageToPngWC img

imageToPngWC :: DynamicImage -> Lb.ByteString
imageToPngWC (ImageRGB8 img) = genericEncodePngWC Nothing Nothing PngTrueColour mempty img
imageToPngWC _ = LbU.pack "Unknown error"

-- ~ Copy & Paste from J.P ============================

-- -- Codec.Picture.VectorByteConversion
mkBS :: ForeignPtr Word8 -> Int -> Int -> BI.ByteString
#if MIN_VERSION_bytestring(0,11,0)
mkBS fptr off = S.BS (fptr `S.plusForeignPtr` off)
#else
mkBS = BI.PS
#endif

blitVector :: VS.Vector Word8 -> Int -> Int -> B.ByteString
blitVector vec atIndex = mkBS ptr (offset + atIndex)
  where (ptr, offset, _length) = VS.unsafeToForeignPtr vec

toByteString :: forall a. (Storable a) => VS.Vector a -> B.ByteString
toByteString vec = mkBS (castForeignPtr ptr) offset (len * size)
  where (ptr, offset, len) = VS.unsafeToForeignPtr vec
        size = sizeOf (undefined :: a)
-- --

preparePalette :: Palette -> PngRawChunk
preparePalette pal = PngRawChunk
  { chunkLength = fromIntegral $ imageWidth pal * 3
  , chunkType   = pLTESignature
  , chunkCRC    = pngComputeCrc [pLTESignature, binaryData]
  , chunkData   = binaryData
  }
  where binaryData = Lb.fromChunks [toByteString $ imageData pal]

preparePaletteAlpha :: VS.Vector Pixel8 -> PngRawChunk
preparePaletteAlpha alphaPal = PngRawChunk
  { chunkLength = fromIntegral $ VS.length alphaPal
  , chunkType   = tRNSSignature
  , chunkCRC    = pngComputeCrc [tRNSSignature, binaryData]
  , chunkData   = binaryData
  }
  where binaryData = Lb.fromChunks [toByteString alphaPal]

type PaletteAlpha = VS.Vector Pixel8

preparePngHeader :: Image a -> PngImageType -> Word8 -> PngIHdr
preparePngHeader (Image { imageWidth = w, imageHeight = h }) imgType depth = PngIHdr
  { width             = fromIntegral w
  , height            = fromIntegral h
  , bitDepth          = depth
  , colourType        = imgType
  , compressionMethod = 0
  , filterMethod      = 0
  , interlaceMethod   = PngNoInterlace
  }


endChunk :: PngRawChunk
endChunk = mkRawChunk iENDSignature mempty

prepareIDatChunk :: Lb.ByteString -> PngRawChunk
prepareIDatChunk = mkRawChunk iDATSignature

-- ~ ============================

genericEncodePngWC :: forall px. (Pixel px, PixelBaseComponent px ~ Word8)
                 => Maybe Palette
                 -> Maybe PaletteAlpha
                 -> PngImageType -> Metadatas -> Image px
                 -> Lb.ByteString
genericEncodePngWC palette palAlpha imgKind metas
                 image@(Image { imageWidth = w, imageHeight = h, imageData = arr }) =
  encode PngRawImage { header = hdr
                     , chunks = encodeMetadatas metas
                              <> paletteChunk
                              <> transpChunk
                              <> [ prepareIDatChunk imgEncodedData
                                 , endChunk
                                 ]}
  where
    hdr = preparePngHeader image imgKind 8
    zero = B.singleton 0
    compCount = componentCount (undefined :: px)

    paletteChunk = case palette of
      Nothing -> []
      Just p -> [preparePalette p]

    transpChunk = case palAlpha of
      Nothing -> []
      Just p -> [preparePaletteAlpha p]

    lineSize = compCount * w
    encodeLine line = blitVector arr (line * lineSize) lineSize
    -- actually all changes was only for CompressionLevel 0
    imgEncodedData = Z.compressWith Z.defaultCompressParams { Z.compressLevel = Z.CompressionLevel 0}
        . Lb.fromChunks
        $ concat [[zero, encodeLine line] | line <- [0 .. h - 1]]