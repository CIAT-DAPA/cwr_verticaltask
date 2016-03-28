using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace ReadFileToLineNumber
{
    class Program
    {
        static void Main(string[] args)
        {
            try
            {
                string file = @"D:\Work\Source Code\crosscheck\Pending\Occurrence_data_GBIF-May2014\write.txt";
                string destination = @"D:\Work\Source Code\crosscheck\Pending\Occurrence_data_GBIF-May2014\write-transform.txt";
                string destinationResumen = @"D:\Work\Source Code\crosscheck\Pending\Occurrence_data_GBIF-May2014\write-transform-resumen.txt";

                string replace = "\t";
                string replaceWith = "|";
                //string header = "publishingCountry|basisOfRecord|scientificName|kingdom|phylum|class|order|family|genus|species|genericName|specificEpithet|taxonRank|decimalLongitude|decimalLatitude|geodeticDatum|countryCode|country|gbifID|institutionCode|catalogNumber|recordedBy|locality|identifiedBy|collectionCode|references|datasetName|elevation|datasetKey";
                string header = "x1_taxon|x1_family|gbif_genus|gbif_species|x1_genus|x1_sp1|gbif_rank|longitude|latitude|iso|country|old_id|institute_id|unique_number|collector|locality|x1_detby|collection_code|references|collection|alt|datasetKey";
                //gbif_genus,gbif_species,collection_code,references,datasetKey

                string temp, line;
                string[] fields;

                int[] omit = new int[] { 0,1,3,4,5,6,15 };

                long lineNumber = 0;
                long i = 0;

                StreamReader reader = new StreamReader(file);
                StreamWriter writer = new StreamWriter(destination, true);
                
                if(!string.IsNullOrEmpty(header))
                    writer.WriteLine(header);
                while ((temp = reader.ReadLine()) != null)
                {
                    if (lineNumber != 0 && i > lineNumber)
                        break;
                    fields = (string.IsNullOrEmpty(replace) ? temp : temp.Replace(replace, replaceWith)).Split(replaceWith[0]);
                    line="";
                    for (int k = 0; k < fields.Length;k++ )
                    {
                        if (!omit.Contains(k))
                            line += fields[k] + replaceWith;
                    }
                    writer.WriteLine(line);
                    i += 1;
                    Console.WriteLine("Row: " + i);
                }                
                writer.Close();

                StreamWriter writer2 = new StreamWriter(destinationResumen, true);
                writer2.WriteLine("Rows: " + i.ToString());
                writer2.Close();

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
