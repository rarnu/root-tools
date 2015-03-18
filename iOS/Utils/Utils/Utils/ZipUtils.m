#import "ZipUtils.h"

@implementation ZipUtils

-(void) unzip {
    [NSThread detachNewThreadSelector:@selector(doUncompress) toTarget:self withObject:nil];
}

-(void) unzipFile:(NSString *)file {
    [NSThread detachNewThreadSelector:@selector(doUncomparessFile:) toTarget:self withObject:file];
}

-(void) doUncompress {
    BOOL b = NO;
    if ([self.delegate respondsToSelector:@selector(zipWillUnzip)]) {
        b = [self.delegate zipWillUnzip];
    }
    if (b) {
        ZipArchive * za = [[ZipArchive alloc] init];
        NSNumber * ret = [NSNumber numberWithBool:NO];
        if ([za UnzipOpenFile: self.archiveFile]) {
            BOOL succ = [za UnzipFileTo:self.extractPath overWrite:YES];
            [za UnzipCloseFile];
            ret = [NSNumber numberWithBool:succ];
        }
        [self performSelectorOnMainThread:@selector(unzipCallback:) withObject:ret waitUntilDone:YES];
    }
}

-(void) doUncomparessFile:(NSString *)file {
    BOOL b = NO;
    if ([self.delegate respondsToSelector:@selector(zipWillUnzip)]) {
        b = [self.delegate zipWillUnzip];
    }
    if (b) {
        ZipArchive * za = [[ZipArchive alloc] init];
        NSNumber * ret = [NSNumber numberWithBool:NO];
        
        if ([za UnzipOpenFile: self.archiveFile]) {
            BOOL succ = [za UnzipFileTo:self.extractPath overWrite:YES];
            [za UnzipCloseFile];
            
            NSArray * unzipped = [za unzippedFiles];
            for (NSString * s in unzipped) {
                if ([s isEqual:file]) {
                    ret = [NSNumber numberWithBool:succ];
                }
            }
        }
        [self performSelectorOnMainThread:@selector(unzipCallback:) withObject:ret waitUntilDone:YES];
    }
}

-(void) unzipCallback: (NSNumber *)ret {
    if ([self.delegate respondsToSelector:@selector(ziputils:unzipCompleted:)]) {
        [self.delegate ziputils:self unzipCompleted:[ret boolValue]];
    }
}

-(void) zip:(NSString *)path {
    [NSThread detachNewThreadSelector:@selector(doCompress:) toTarget:self withObject:path];
}
-(void) zipFiles:(NSArray *)files {
    [NSThread detachNewThreadSelector:@selector(doCompressFiles:) toTarget:self withObject:files];
}
-(void) doCompress:(NSString *)path {
    BOOL b = NO;
    @try {
        ZipArchive * za = [[ZipArchive alloc] init];
        [za CreateZipFile2:self.archiveFile];
        NSArray * arr = [[NSFileManager defaultManager] contentsOfDirectoryAtPath:path error:nil];
        for (NSString * file in arr) {
            [za addFileToZip:file newname:file];
        }
        [za CloseZipFile2];
        b = YES;
    }
    @catch (NSException *exception) {
        
    }
    [self performSelectorOnMainThread:@selector(zipCallback:) withObject:[NSNumber numberWithBool:b] waitUntilDone:YES];
    
}
-(void) doCompressFiles:(NSArray *)files {
    BOOL b = NO;
    @try {
        ZipArchive * za = [[ZipArchive alloc] init];
        [za CreateZipFile2:self.archiveFile];
        for (NSString * file in files) {
            [za addFileToZip:file newname:file];
        }
        [za CloseZipFile2];
        b = YES;
    }
    @catch (NSException *exception) {
        
    }
    [self performSelectorOnMainThread:@selector(zipCallback:) withObject:[NSNumber numberWithBool:b] waitUntilDone:YES];
}

-(void) zipCallback: (NSNumber *)ret {
    if ([self.delegate respondsToSelector:@selector(ziputils:zipCompleted:)]) {
        [self.delegate ziputils:self zipCompleted:[ret boolValue]];
    }
}


@end
