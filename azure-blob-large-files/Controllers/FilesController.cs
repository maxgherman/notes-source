using System;
using System.IO;
using System.Threading.Tasks;
using App.Models;
using App.Services;
using Microsoft.AspNetCore.Mvc;
using Microsoft.Extensions.Primitives;

namespace App.Controllers
{
    [ApiController]
    [Route("files")]
    public class FilesController : ControllerBase
    {
        private readonly IFileStorageService filesStorage;
     
        public FilesController(IFileStorageService filesStorage)
        {
            this.filesStorage = filesStorage;
        }
        
        [HttpPost("chunk")]
        public async Task<IActionResult> UploadChunk()
        {
            var fileId =
                Request.Headers.TryGetValue("X-Content-Id", out StringValues contentId) ?
                Guid.Parse(contentId) : Guid.NewGuid();

            var blockId = long.Parse(Request.Headers["X-Chunk-Id"]);

            using(var stream = new MemoryStream())
            {
                await Request.Body.CopyToAsync(stream);
                stream.Position = 0;
                
                await filesStorage.UploadBlockAsync(fileId, blockId, stream);
            }
         
            return Ok(new { FileId = fileId });
        }
    
        [HttpPost("{id}")]
        public async Task<IActionResult> CommitFile(
            Guid id,
            [FromBody] FileMetadataModel metadata)
        {
            await filesStorage.CommitBlocksAsync(id, metadata.ToStorage());

            return CreatedAtAction(nameof(GetFile), new { id }, null);
        }

        [HttpGet("{id}")]
        public async Task<IActionResult> GetFile(Guid id)
        {
            var data = await filesStorage.DownloadAsync(id);

            return File(
                data.Stream,
                "application/force-download",
                data.Metadata.FileName,
                enableRangeProcessing: true);
        }
    }
}