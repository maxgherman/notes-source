using System;
using System.IO;
using System.Threading.Tasks;

namespace App.Services
{
    public interface IFileStorageService
    {
        Task CreateContainerAsync();
        
        Task UploadBlockAsync(Guid fileId, long blockId, Stream block);

        Task CommitBlocksAsync(Guid fileId, FileMetadata metadata);

        Task<(Stream Stream, FileMetadata Metadata)> DownloadAsync(Guid fileId);
    }
}