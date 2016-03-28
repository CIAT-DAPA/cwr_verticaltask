using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace CreateFolderForEveryFile
{
    class Program
    {
        static void Main(string[] args)
        {
            try
            {
                string source = @"C:\Users\hsotelo\Desktop\Uploaded_Occurrences_Source", destination = @"D:\Work\Source Code\crosscheck\", directory, fullFile, fullPath;
                foreach (string file in Directory.EnumerateFiles(source))
                {
                    fullFile = file.Replace(@"C:\Users\hsotelo\Desktop\Uploaded_Occurrences_Source\", "");
                    directory = file.Substring(0, file.IndexOf(".")).Replace(@"C:\Users\hsotelo\Desktop\Uploaded_Occurrences_Source\", "");
                    fullPath = destination + directory;
                    Directory.CreateDirectory(fullPath);
                    File.Copy(file, fullPath + "\\" + fullFile);
                    Console.WriteLine(fullPath + "\\" + fullFile);

                }
                Console.ReadLine();
            }
            catch (Exception ex)
            {
                Console.WriteLine(ex);
                Console.ReadLine();
            }
        }
    }
}
