using System.ComponentModel.DataAnnotations;
using App.Services;

namespace App.Models
{
    public class FileMetadataModel
    {
        [Required]
        public string FileName { get; set; }

        [Required]
        public long? Size { get; set; }

        public FileMetadata ToStorage()
        {
            return new FileMetadata
            {
                FileName = this.FileName,
                Size = this.Size.Value
            };
        }
    }
}