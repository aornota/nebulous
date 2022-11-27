module Aornota.Nebulous.Utilities

open Avalonia
open Avalonia.Media
open Avalonia.Platform
open System.Drawing
open System.Drawing.Imaging

let isDebug =
#if DEBUG
    true
#else
    false
#endif

// Adapted from http://www.fssnip.net/sn/title/Bitmap-Primitives-Helpers.

let initArray array (fn: int -> int -> Color) =
    Array2D.init (array |> Array2D.length1) (array |> Array2D.length2) fn

let toBitmap array =
    let bitmap = new Bitmap(array |> Array2D.length1, array |> Array2D.length2)

    array |> Array2D.iteri (fun x y colour -> bitmap.SetPixel(x, y, colour))

    bitmap

// Adapted from https://github.com/AvaloniaUI/Avalonia/discussions/5908.

let toAvaloniaBitmap (bitmap: Bitmap) =
    let data =
        bitmap.LockBits(
            Rectangle(0, 0, bitmap.Width, bitmap.Height),
            ImageLockMode.ReadOnly,
            PixelFormat.Format32bppArgb
        )

    let converted =
        new Imaging.Bitmap(
            PixelFormat.Bgra8888,
            AlphaFormat.Premul,
            data.Scan0,
            PixelSize(data.Width, data.Height),
            Vector(96, 96),
            data.Stride
        )

    bitmap.UnlockBits(data)
    converted
