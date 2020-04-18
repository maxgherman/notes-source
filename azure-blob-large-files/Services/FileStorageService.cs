using System;
using System.IO;
using System.Linq;
using System.Security.Cryptography;
using System.Text;
using System.Threading.Tasks;
using App.Configuration;
using Azure.Storage.Blobs;
using Azure.Storage.Blobs.Specialized;

namespace App.Services
{
    public class FileStorageService : IFileStorageService
    {
        private readonly BlobStorageConfig blobStorageConfig;

        public FileStorageService(BlobStorageConfig blobStorageConfig)
        {
            this.blobStorageConfig = blobStorageConfig;
        }
        
        public async Task UploadBlockAsync(Guid fileId, long blockId, Stream block)
        {
            var client = CreateBlockClient(fileId);

            using (var md5 = MD5.Create())
            {
                var hash = md5.ComputeHash(block);
                block.Position = 0;

                await client.StageBlockAsync(
                    GenerateBlobBlockId(blockId),
                    block,
                    hash
                );
            }
        }

        public async Task CommitBlocksAsync(Guid fileId, FileMetadata metadata)
        {
            var client = CreateBlockClient(fileId);
            
            var blockList = await client.GetBlockListAsync();
            var blobBlockIds = blockList.Value.UncommittedBlocks
                .Select(item => item.Name).ToArray();

            await client.CommitBlockListAsync(
                OrderBlobBlockIds(blobBlockIds),
                null,
                metadata.ToStorageMetadata()
            );
        }

        public async Task<(Stream Stream, FileMetadata Metadata)> DownloadAsync(Guid fileId)
        {
            var client = CreateBlockClient(fileId);
            var blob = await client.DownloadAsync();
            var properties = await client.GetPropertiesAsync();

            return (
                Stream: blob.Value.Content,
                Metadata: FileMetadata.FromStorageMetadata(properties.Value.Metadata)
            );
        }

        public async Task CreateContainerAsync()
        {
            var serviceClient = new BlobServiceClient(blobStorageConfig.ConnectionString);
            var blobClient = serviceClient.GetBlobContainerClient(blobStorageConfig.ContainerName);

            await blobClient.CreateIfNotExistsAsync();
        }
        
        private string GenerateBlobBlockId(long blockId)
        {
            return Convert.ToBase64String(
                Encoding.UTF8.GetBytes(
                    blockId.ToString("d20")
                )
            );
        }

        private string[] OrderBlobBlockIds(string[] blobBlockIds)
        {
            return blobBlockIds.Select(Convert.FromBase64String)
                .Select(Encoding.UTF8.GetString)
                .Select(long.Parse)
                .OrderBy(_ => _)
                .Select(GenerateBlobBlockId)
                .ToArray();
        }

        private BlockBlobClient CreateBlockClient(Guid fileId)
        {
            var serviceClient = new BlobServiceClient(blobStorageConfig.ConnectionString);
            var blobClient = serviceClient.GetBlobContainerClient(blobStorageConfig.ContainerName);

            return new BlockBlobClient(
                blobStorageConfig.ConnectionString, blobStorageConfig.ContainerName, fileId.ToString());
        }
    }
}