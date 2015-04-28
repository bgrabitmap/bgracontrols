using System;
using System.Runtime.InteropServices;

namespace BGRABitmapLibrary
{
    public class BGRABitmap
    {
        /* Constructors */

        [DllImport("bgrabitmap")]
        public static extern void bgraCreate(int id);

        [DllImport("bgrabitmap")]
        public static extern void bgraCreateWithSize(int id, int AWidth, int AHeight);

        [DllImport("bgrabitmap")]
        public static extern void bgraDestroy(int id);

        [DllImport("bgrabitmap")]
        public static extern int bgraGetHighestID();

        /* Color */

        [DllImport("bgrabitmap")]
        public static extern long rgb(byte red, byte green, byte blue);

        [DllImport("bgrabitmap")]
        public static extern long rgba(byte red, byte green, byte blue, byte alpha);

        [DllImport("bgrabitmap")]
        public static extern byte getBlue(long AColor);

        [DllImport("bgrabitmap")]
        public static extern byte getGreen(long AColor);

        [DllImport("bgrabitmap")]
        public static extern byte getRed(long AColor);

        [DllImport("bgrabitmap")]
        public static extern byte getAlpha(long AColor);

        [DllImport("bgrabitmap")]
        public static extern long setBlue(long AColor, byte AValue);

        [DllImport("bgrabitmap")]
        public static extern long setGreen(long AColor, byte AValue);

        [DllImport("bgrabitmap")]
        public static extern long setRed(long AColor, byte AValue);

        [DllImport("bgrabitmap")]
        public static extern long setAlpha(long AColor, byte AValue);

        /* Pixels */

        [DllImport("bgrabitmap")]
        public static extern void bgraFill(int id, long AColor);

        [DllImport("bgrabitmap")]
        public static extern void bgraSetPixel(int id, int x, int y, long AColor);

        [DllImport("bgrabitmap")]
        public static extern long bgraGetPixel(int id, int x, int y);

        /* Files */

        [DllImport("bgrabitmap")]
        public static extern void bgraCreateFromFile(int id, [MarshalAs(UnmanagedType.LPWStr)]string AFileName);

        [DllImport("bgrabitmap")]
        public static extern void bgraSaveToFile(int id, [MarshalAs(UnmanagedType.LPWStr)]string AFileName);
    }
}
