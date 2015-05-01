using System;
using System.Drawing;
using System.Runtime.InteropServices;

namespace BGRABitmapLibrary
{
    public static class BGRABitmap
    {
        /* Types */
        
		public enum MedianOption : byte { None, LowSmooth, MediumSmooth, HighSmooth };
		
		public enum ResampleFilter : byte { Box, Linear, HalfCosine, Cosine, Bicubic, Mitchell, Spline, Lanczos2, Lanczos3, Lanczos4, BestQuality };
		
		public enum RadialBlurType : byte { Normal, Disk, Corona, Precise, Fast, Box };

        /* Constructors */

        [DllImport("bgrabitmap", EntryPoint = "create")]
        public static extern void Create(int id);

        [DllImport("bgrabitmap", EntryPoint = "createwithsize")]
        public static extern void CreateWithSize(int id, int AWidth, int AHeight);

        [DllImport("bgrabitmap", EntryPoint = "destroy")]
        public static extern void Destroy(int id);

        [DllImport("bgrabitmap", EntryPoint = "destroyall")]
        public static extern void DestroyAll();

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
        public static extern void FilterSmartZoom3(int id, MedianOption Option);
		
		[DllImport("bgrabitmap", EntryPoint = "filtermedian")]
        public static extern void FilterMedian(int id, MedianOption Option);
		
		[DllImport("bgrabitmap", EntryPoint = "filtersmooth")]
        public static extern void FilterSmooth(int id);
		
		[DllImport("bgrabitmap", EntryPoint = "filtersharpen")]
        public static extern void FilterSharpen(int id, Single Amount);
		
        [DllImport("bgrabitmap", EntryPoint = "filtersharpenrect")]
        public static extern void FilterSharpen(int id, Rectangle ABounds, Single Amount);
		
		[DllImport("bgrabitmap", EntryPoint = "filtercontour")]
        public static extern void FilterContour(int id);
		
		[DllImport("bgrabitmap", EntryPoint = "filterpixelate")]
        public static extern void FilterPixelate(int id, int pixelSize, bool useResample, ResampleFilter filter);
		
		[DllImport("bgrabitmap", EntryPoint = "filterblurradial")]
        public static extern void FilterBlurRadial(int id, int radius, RadialBlurType blurType);
		
        [DllImport("bgrabitmap", EntryPoint = "filterblurradialrect")]
        public static extern void FilterBlurRadial(int id, Rectangle ABounds, int radius, RadialBlurType blurType);
		
		[DllImport("bgrabitmap", EntryPoint = "filterblurmotion")]
        public static extern void FilterBlurMotion(int id, int distance, Single angle, bool oriented);
		
        [DllImport("bgrabitmap", EntryPoint = "filterblurmotionrect")]
        public static extern void FilterBlurMotion(int id, Rectangle ABounds, int distance, Single angle, bool oriented);
		
		[DllImport("bgrabitmap", EntryPoint = "filtercustomblur")]
        public static extern void FilterCustomBlur(int id, int mask);
		
        [DllImport("bgrabitmap", EntryPoint = "filtercustomblur")]
        public static extern void FilterCustomBlur(int id, Rectangle ABounds, int mask);
		
		[DllImport("bgrabitmap", EntryPoint = "filteremboss")]
        public static extern void FilterEmboss(int id, Single angle);
		
        [DllImport("bgrabitmap", EntryPoint = "filterembossrect")]
        public static extern void FilterEmboss(int id, Single angle, Rectangle ABounds);
		
		[DllImport("bgrabitmap", EntryPoint = "filterembosshighlight")]
        public static extern void FilterEmboss(int id, bool FillSelection);
		
		[DllImport("bgrabitmap", EntryPoint = "filterembosshighlightborder")]
        public static extern void FilterEmboss(int id, bool FillSelection, uint BorderColor);
		
        [DllImport("bgrabitmap", EntryPoint = "filterembosshighlightborderandoffset")]
        public static extern void FilterEmboss(int id, bool FillSelection, uint BorderColor, Point Offset);
        
        [DllImport("bgrabitmap", EntryPoint = "filtergrayscale")]
        public static extern void FilterGrayscale(int id);
		
        [DllImport("bgrabitmap", EntryPoint = "filtergrayscalerect")]
        public static extern void FilterGrayscale(int id, Rectangle ABounds);
		
		[DllImport("bgrabitmap", EntryPoint = "filternormalize")]
        public static extern void FilterNormalize(int id, bool eachChannel);

        [DllImport("bgrabitmap", EntryPoint = "filternormalizerect")]
        public static extern void FilterNormalize(int id, Rectangle ABounds, bool eachChannel);
		
        [DllImport("bgrabitmap", EntryPoint = "filterrotate")]
        public static extern void FilterRotate(int id, PointF origin, Single angle, bool correctBlur);
		
		[DllImport("bgrabitmap", EntryPoint = "filtersphere")]
        public static extern void FilterSphere(int id);
		
        [DllImport("bgrabitmap", EntryPoint = "filtertwirl")]
        public static extern void FilterTwirl(int id, Point ACenter, Single ARadius, Single ATurn, Single AExponent);
		
        [DllImport("bgrabitmap", EntryPoint = "filtertwirlrect")]
        public static extern void FilterTwirl(int id, Rectangle ABounds, Point ACenter, Single ARadius, Single ATurn, Single AExponent);
		
		[DllImport("bgrabitmap", EntryPoint = "filtercylinder")]
        public static extern void FilterCylinder(int id);
		
		[DllImport("bgrabitmap", EntryPoint = "filterplane")]
        public static extern void FilterPlane(int id);
    }
}
