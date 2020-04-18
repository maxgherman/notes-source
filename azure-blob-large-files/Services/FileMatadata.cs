using System.Collections.Generic;

namespace App.Services
{
    public class FileMetadata
    {
        public string FileName { get; set; }

        public long Size { get; set; }

        public IDictionary<string, string> ToStorageMetadata()
        {
            return new Dictionary<string, string>
            {
                ["fileName"] = FileName,
                ["originalSize"] = Size.ToString()
            };
        }

        public static FileMetadata FromStorageMetadata(IDictionary<string, string> metadata)
        {
            return new FileMetadata
            {
                FileName = metadata["fileName"],
                Size = long.Parse(metadata["originalSize"])
            };
        }
    }
}