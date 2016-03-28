using System;
using System.Collections.Generic;
using System.Drawing;
using System.Drawing.Imaging;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace JoinImages
{
    class Program
    {
        static void Main(string[] args)
        {
            try
            {
                string directory, pattern1, pattern2, destination;
                string file1, file2;
                Bitmap bmp1, bmp2, bmpDestination;
                bool vertical;

                vertical = false;
                //directory = @"C:\Users\hsotelo\Desktop\Imagenes";
                directory = @"C:\Users\hsotelo\Desktop\others";

                pattern1 = "_i.png";
                pattern2 = "_d.png";
                foreach (var file in Directory.EnumerateFiles(directory, "*" + pattern1))
                {
                    Console.WriteLine("Check file: " + file);
                    file1 = file;
                    file2 = file.Replace(pattern1, pattern2);

                    bmp1 = new Bitmap(file1);

                    if (File.Exists(file2))
                    {
                        Console.WriteLine("Generate image");
                        
                        bmp2 = new Bitmap(file2);
                        if (!vertical)
                        {
                            bmpDestination = new Bitmap(bmp1.Width + bmp2.Width, (int)Math.Max(bmp1.Height, bmp2.Height), PixelFormat.Format24bppRgb);
                            for (int x = 0; x < bmp1.Width; x++)
                            {
                                for (int y = 0; y < bmp1.Height; y++)
                                    bmpDestination.SetPixel(x, y, bmp1.GetPixel(x, y));
                            }
                            for (int x = 0; x < bmp2.Width; x++)
                            {
                                for (int y = 0; y < bmp2.Height; y++)
                                    bmpDestination.SetPixel(x + bmp1.Width, y, bmp2.GetPixel(x, y));
                            }
                        }
                        else
                        {
                            bmpDestination = new Bitmap((int)Math.Max(bmp1.Width, bmp2.Width), bmp1.Height + bmp2.Height, PixelFormat.Format24bppRgb);
                            for (int x = 0; x < bmp1.Width; x++)
                            {
                                for (int y = 0; y < bmp1.Height; y++)
                                    bmpDestination.SetPixel(x, y, bmp1.GetPixel(x, y));
                            }
                            for (int x = 0; x < bmp2.Width; x++)
                            {
                                for (int y = 0; y < bmp2.Height; y++)
                                    bmpDestination.SetPixel(x, y + bmp1.Height, bmp2.GetPixel(x, y));
                            }
                        }
                        Graphics gr = Graphics.FromImage(bmpDestination);
                        gr.DrawImageUnscaled(bmpDestination, 0, 0);
                        gr.DrawString("A)", new Font("Arial", 60, FontStyle.Bold), Brushes.Black, new PointF(10, 10));
                        gr.DrawString("B)", new Font("Arial", 60, FontStyle.Bold), Brushes.Black, new PointF(10 + bmp1.Width, 10));
                        destination = file.Replace(pattern1, "_final.png");
                        bmpDestination.Save(destination);
                        Console.WriteLine("Image save: " + destination);
                    }
                    else
                    {
                        bmpDestination = new Bitmap(bmp1.Width,bmp1.Height, PixelFormat.Format24bppRgb);
                        for (int x = 0; x < bmp1.Width; x++)
                        {
                            for (int y = 0; y < bmp1.Height; y++)
                                bmpDestination.SetPixel(x, y, bmp1.GetPixel(x, y));
                        }
                        Graphics gr = Graphics.FromImage(bmpDestination);
                        gr.DrawImageUnscaled(bmpDestination, 0, 0);
                        gr.DrawString("A)", new Font("Arial", 60, FontStyle.Bold), Brushes.Black, new PointF(10, 10));
                        destination = file.Replace(pattern1, "_final.png");
                        bmpDestination.Save(destination);
                        Console.WriteLine("Image save: " + destination);

                    }
                }
            }
            catch (Exception ex)
            {
                Console.WriteLine(ex.Message);
                Console.WriteLine(ex.StackTrace);
                Console.ReadLine();
            }
        }
    }
}
