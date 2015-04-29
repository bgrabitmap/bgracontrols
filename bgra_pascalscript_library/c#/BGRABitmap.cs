using System;
using System.Runtime.InteropServices;

namespace BGRABitmapLibrary
{
    public class BGRABitmap
    {
        /* Constructors */

        [DllImport("bgrabitmap")]
        public static extern void Create(int id);

        [DllImport("bgrabitmap")]
        public static extern void CreateWithSize(int id, int AWidth, int AHeight);

        [DllImport("bgrabitmap")]
        public static extern void Destroy(int id);

        [DllImport("bgrabitmap")]
        public static extern int GetHighestID();

        /* Files */

        [DllImport("bgrabitmap")]
        public static extern void CreateFromFile(int id, [MarshalAs(UnmanagedType.LPWStr)]string AFileName);

        [DllImport("bgrabitmap")]
        public static extern void SaveToFile(int id, [MarshalAs(UnmanagedType.LPWStr)]string AFileName);

        /* Color */

        [DllImport("bgrabitmap")]
        public static extern uint rgb(byte red, byte green, byte blue);

        [DllImport("bgrabitmap")]
        public static extern uint rgba(byte red, byte green, byte blue, byte alpha);

        [DllImport("bgrabitmap")]
        public static extern byte getBlue(uint AColor);

        [DllImport("bgrabitmap")]
        public static extern byte getGreen(uint AColor);

        [DllImport("bgrabitmap")]
        public static extern byte getRed(uint AColor);

        [DllImport("bgrabitmap")]
        public static extern byte getAlpha(uint AColor);

        [DllImport("bgrabitmap")]
        public static extern uint setBlue(uint AColor, byte AValue);

        [DllImport("bgrabitmap")]
        public static extern uint setGreen(uint AColor, byte AValue);

        [DllImport("bgrabitmap")]
        public static extern uint setRed(uint AColor, byte AValue);

        [DllImport("bgrabitmap")]
        public static extern uint setAlpha(uint AColor, byte AValue);

        /* Pixels */

        [DllImport("bgrabitmap")]
        public static extern void Fill(int id, uint AColor);

        [DllImport("bgrabitmap")]
        public static extern void SetPixel(int id, int x, int y, uint AColor);

        [DllImport("bgrabitmap")]
        public static extern uint GetPixel(int id, int x, int y);

        /* Filters */

        [DllImport("bgrabitmap")]
        public static extern uint FilterGrayscale(int id);
    }
}
