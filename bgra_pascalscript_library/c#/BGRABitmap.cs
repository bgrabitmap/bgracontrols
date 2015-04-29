using System;
using System.Runtime.InteropServices;

namespace BGRABitmapLibrary
{
    public class BGRABitmap
    {
        /* Types */
        public enum TMedianOption : byte { moNone, moLowSmooth, moMediumSmooth, moHighSmooth };

        /* Constructors */

        [DllImport("bgrabitmap", EntryPoint = "create")]
        public static extern void Create(int id);

        [DllImport("bgrabitmap", EntryPoint = "createwithsize")]
        public static extern void CreateWithSize(int id, int AWidth, int AHeight);

        [DllImport("bgrabitmap", EntryPoint = "destroy")]
        public static extern void Destroy(int id);

        [DllImport("bgrabitmap", EntryPoint = "gethighestid")]
        public static extern int GetHighestID();

        /* Files */

        [DllImport("bgrabitmap", EntryPoint = "createfromfile")]
        public static extern void CreateFromFile(int id, [MarshalAs(UnmanagedType.LPWStr)]string AFileName);

        [DllImport("bgrabitmap", EntryPoint = "savetofile")]
        public static extern void SaveToFile(int id, [MarshalAs(UnmanagedType.LPWStr)]string AFileName);

        /* Color */

        [DllImport("bgrabitmap", EntryPoint = "rgb")]
        public static extern uint rgb(byte red, byte green, byte blue);

        [DllImport("bgrabitmap", EntryPoint = "rgba")]
        public static extern uint rgba(byte red, byte green, byte blue, byte alpha);

        [DllImport("bgrabitmap", EntryPoint = "getblue")]
        public static extern byte getBlue(uint AColor);

        [DllImport("bgrabitmap", EntryPoint = "getgreen")]
        public static extern byte getGreen(uint AColor);

        [DllImport("bgrabitmap", EntryPoint = "getred")]
        public static extern byte getRed(uint AColor);

        [DllImport("bgrabitmap", EntryPoint = "getalpha")]
        public static extern byte getAlpha(uint AColor);

        [DllImport("bgrabitmap", EntryPoint = "setblue")]
        public static extern uint setBlue(uint AColor, byte AValue);

        [DllImport("bgrabitmap", EntryPoint = "setgreen")]
        public static extern uint setGreen(uint AColor, byte AValue);

        [DllImport("bgrabitmap", EntryPoint = "setred")]
        public static extern uint setRed(uint AColor, byte AValue);

        [DllImport("bgrabitmap", EntryPoint = "setalpha")]
        public static extern uint setAlpha(uint AColor, byte AValue);

        /* Pixels */

        [DllImport("bgrabitmap", EntryPoint = "fill")]
        public static extern void Fill(int id, uint AColor);

        [DllImport("bgrabitmap", EntryPoint = "setpixel")]
        public static extern void SetPixel(int id, int x, int y, uint AColor);

        [DllImport("bgrabitmap", EntryPoint = "getpixel")]
        public static extern uint GetPixel(int id, int x, int y);

        /* Filters */

        [DllImport("bgrabitmap", EntryPoint = "filtersmartzoom3")]
        public static extern void FilterSmartZoom3(int id, TMedianOption Option);

        [DllImport("bgrabitmap", EntryPoint = "filtergrayscale")]
        public static extern void FilterGrayscale(int id);
    }
}
