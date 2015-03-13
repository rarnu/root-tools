#import "ZipArchive.h"
#import "zlib.h"
#import "zconf.h"
#include "minizip/zip.h"
#include "minizip/unzip.h"


@interface NSFileManager(ZipArchive)
- (NSDictionary *)_attributesOfItemAtPath:(NSString *)path followingSymLinks:(BOOL)followingSymLinks error:(NSError **)error;
@end

@interface ZipArchive ()

-(void) OutputErrorMessage:(NSString*) msg;
-(BOOL) OverWrite:(NSString*) file;
-(NSDate*) Date1980;

@property (nonatomic,copy) NSString* password;
@end

@implementation ZipArchive
@synthesize delegate = _delegate;
@synthesize numFiles = _numFiles;
@synthesize password = _password;
@synthesize unzippedFiles = _unzippedFiles;
@synthesize progressBlock = _progressBlock;
@synthesize stringEncoding = _stringEncoding;

-(id) init {
    return [self initWithFileManager:[NSFileManager defaultManager]];
}

-(id) initWithFileManager:(NSFileManager*) fileManager {
	if( self=[super init] ) {
		_zipFile = NULL;
        _fileManager = fileManager;
        self.stringEncoding = NSUTF8StringEncoding;
        self.compression = ZipArchiveCompressionDefault;
	}
	return self;
}

-(void) dealloc {
	[self CloseZipFile2];
    [self UnzipCloseFile];
}

-(BOOL) CreateZipFile2:(NSString*) zipFile {
    return [self CreateZipFile2:zipFile append:NO];
}

-(BOOL) CreateZipFile2:(NSString*) zipFile append:(BOOL)isAppend {
    _zipFile = zipOpen( (const char*)[zipFile UTF8String], (isAppend ? APPEND_STATUS_ADDINZIP : APPEND_STATUS_CREATE) );
	if( !_zipFile ) 
		return NO;
	return YES;
}

-(BOOL) CreateZipFile2:(NSString*) zipFile Password:(NSString*) password {
	self.password = password;
	return [self CreateZipFile2:zipFile];
}

-(BOOL) CreateZipFile2:(NSString*) zipFile Password:(NSString*) password append:(BOOL)isAppend {
    self.password = password;
    return [self CreateZipFile2:zipFile append:isAppend];
}

-(BOOL) addFileToZip:(NSString*) file newname:(NSString*) newname; {
    NSData *data = [NSData dataWithContentsOfFile:file];
    NSError* error = nil;
    NSDictionary* attr = [_fileManager _attributesOfItemAtPath:file followingSymLinks:YES error:&error];
    BOOL result = [self addDataToZip:data fileAttributes:attr newname:newname];
    return result;
}


-(BOOL) addDataToZip:(NSData*) data fileAttributes:(NSDictionary *)attr newname:(NSString*) newname {
    if (!data) {
        return NO;
    }
	if( !_zipFile )
		return NO;

	zip_fileinfo zipInfo = {{0}};

	NSDate* fileDate = nil;
    
	if( attr )
		fileDate = (NSDate*)[attr objectForKey:NSFileModificationDate];

	if( fileDate == nil )
        fileDate = [NSDate date];

    
    NSCalendar *gregorianCalendar = [[NSCalendar alloc] initWithCalendarIdentifier:NSCalendarIdentifierGregorian];
    NSDateComponents* components = [gregorianCalendar components:NSCalendarUnitYear | NSCalendarUnitMonth | NSCalendarUnitDay |
                                    NSCalendarUnitHour | NSCalendarUnitMinute | NSCalendarUnitSecond fromDate:fileDate];
    
    zipInfo.tmz_date.tm_sec = (uInt)components.second;
    zipInfo.tmz_date.tm_min = (uInt)components.minute;
    zipInfo.tmz_date.tm_hour = (uInt)components.hour;
    zipInfo.tmz_date.tm_mday = (uInt)components.day;
    zipInfo.tmz_date.tm_mon = (uInt)components.month;
    zipInfo.tmz_date.tm_year = (uInt)components.year;
    
	
	int ret ;
	if( [_password length] == 0 ) {
		ret = zipOpenNewFileInZip( _zipFile,
								  (const char*) [newname cStringUsingEncoding:self.stringEncoding],
								  &zipInfo,
								  NULL,0,
								  NULL,0,
								  NULL,
								  Z_DEFLATED,
								  self.compression );
	} else {
		uLong crcValue = crc32( 0L,NULL, 0L );
		crcValue = crc32( crcValue, (const Bytef*)[data bytes], (unsigned int)[data length] );
		ret = zipOpenNewFileInZip3( _zipFile,
								  (const char*) [newname cStringUsingEncoding:self.stringEncoding],
								  &zipInfo,
								  NULL,0,
								  NULL,0,
								  NULL,
								  Z_DEFLATED,
								  self.compression,
								  0,
								  15,
								  8,
								  Z_DEFAULT_STRATEGY,
								  [_password cStringUsingEncoding:NSASCIIStringEncoding],
								  crcValue );
	}
	if( ret!=Z_OK ) {
		return NO;
	}
	unsigned int dataLen = (unsigned int)[data length];
	ret = zipWriteInFileInZip( _zipFile, (const void*)[data bytes], dataLen);
	if( ret!=Z_OK ) {
		return NO;
	}
	ret = zipCloseFileInZip( _zipFile );
	if( ret!=Z_OK )
		return NO;
	return YES;
}


-(BOOL) CloseZipFile2 {
	self.password = nil;
	if( _zipFile==NULL )
		return NO;
	BOOL ret =  zipClose( _zipFile,NULL )==Z_OK?YES:NO;
	_zipFile = NULL;
	return ret;
}

-(BOOL) UnzipOpenFile:(NSString*) zipFile {
    _unzippedFiles = [[NSMutableArray alloc] initWithCapacity:1];
    
	_unzFile = unzOpen( (const char*)[zipFile UTF8String] );
	if( _unzFile ) {
		unz_global_info  globalInfo = {0};
		if( unzGetGlobalInfo(_unzFile, &globalInfo )==UNZ_OK ) {
            _numFiles = globalInfo.number_entry;
			NSLog(@"%lu entries in the zip file", globalInfo.number_entry);
		}
	}
	return _unzFile!=NULL;
}

-(BOOL) UnzipOpenFile:(NSString*) zipFile Password:(NSString*) password {
	self.password = password;
	return [self UnzipOpenFile:zipFile];
}


-(BOOL) UnzipFileTo:(NSString*) path overWrite:(BOOL) overwrite {
	BOOL success = YES;
    int index = 0;
    int progress = -1;
	int ret = unzGoToFirstFile( _unzFile );
	unsigned char		buffer[4096] = {0};
	if( ret!=UNZ_OK ) {
		[self OutputErrorMessage:@"Failed"];
	}
    
	const char* password = [_password cStringUsingEncoding:NSASCIIStringEncoding];
	
	do{
        @autoreleasepool {
            if( [_password length]==0 )
                ret = unzOpenCurrentFile( _unzFile );
            else
                ret = unzOpenCurrentFilePassword( _unzFile, password );
            if( ret!=UNZ_OK ) {
                [self OutputErrorMessage:@"Error occurs"];
                success = NO;
                break;
            }
            int read ;
            unz_file_info	fileInfo ={0};
            ret = unzGetCurrentFileInfo(_unzFile, &fileInfo, NULL, 0, NULL, 0, NULL, 0);
            if( ret!=UNZ_OK ) {
                [self OutputErrorMessage:@"Error occurs while getting file info"];
                success = NO;
                unzCloseCurrentFile( _unzFile );
                break;
            }
            char* filename = (char*) malloc( fileInfo.size_filename +1 );
            unzGetCurrentFileInfo(_unzFile, &fileInfo, filename, fileInfo.size_filename + 1, NULL, 0, NULL, 0);
            filename[fileInfo.size_filename] = '\0';
            
            NSString * strPath = [NSString stringWithCString:filename encoding:self.stringEncoding];
            BOOL isDirectory = NO;
            if( filename[fileInfo.size_filename-1]=='/' || filename[fileInfo.size_filename-1]=='\\')
                isDirectory = YES;
            free( filename );
            if( [strPath rangeOfCharacterFromSet:[NSCharacterSet characterSetWithCharactersInString:@"/\\"]].location!=NSNotFound ) {
                strPath = [strPath stringByReplacingOccurrencesOfString:@"\\" withString:@"/"];
            }
            NSString* fullPath = [path stringByAppendingPathComponent:strPath];
            
            if( isDirectory )
                [_fileManager createDirectoryAtPath:fullPath withIntermediateDirectories:YES attributes:nil error:nil];
            else
                [_fileManager createDirectoryAtPath:[fullPath stringByDeletingLastPathComponent] withIntermediateDirectories:YES attributes:nil error:nil];
            
            FILE* fp = NULL;
            do {
                read = unzReadCurrentFile(_unzFile, buffer, 4096);
                if (read >= 0) {
                    if (fp == NULL) {
                        if( [_fileManager fileExistsAtPath:fullPath] && !isDirectory && !overwrite ) {
                            if( ![self OverWrite:fullPath] ) {
                                break;
                            }
                        }
                        if (!isDirectory) {
                            fp = fopen( (const char*)[fullPath UTF8String], "wb");
                            if (fp == NULL) {
                                [self OutputErrorMessage:@"Failed to open output file for writing"];
                                break;
                            }
                        }
                    }
                    fwrite(buffer, read, 1, fp );
                } else {
                    ret = read;
                    success = NO;
                    [self OutputErrorMessage:@"Failed to read zip file"];
                }
            } while (read > 0);
            
            if (fp) {
                fclose( fp );
                [(NSMutableArray*)_unzippedFiles addObject:fullPath];
                
                if( fileInfo.tmu_date.tm_year!=0 ){
                    NSDateComponents* components = [[NSDateComponents alloc] init];
                    components.second = fileInfo.tmu_date.tm_sec;
                    components.minute = fileInfo.tmu_date.tm_min;
                    components.hour = fileInfo.tmu_date.tm_hour;
                    components.day = fileInfo.tmu_date.tm_mday;
                    components.month = fileInfo.tmu_date.tm_mon + 1;
                    components.year = fileInfo.tmu_date.tm_year;
                    
                    NSCalendar *gregorianCalendar = [[NSCalendar alloc] initWithCalendarIdentifier:NSCalendarIdentifierGregorian];
                    NSDate* orgDate = [gregorianCalendar dateFromComponents:components];
                    
                    NSDictionary* attr = [NSDictionary dictionaryWithObject:orgDate forKey:NSFileModificationDate];
                    if( attr ) {
                        if( ![_fileManager setAttributes:attr ofItemAtPath:fullPath error:nil] ){
                            NSLog(@"Failed to set attributes");
                        }
                        
                    }
                    orgDate = nil;
                }
                
            }
            
            if (ret == UNZ_OK) {
                ret = unzCloseCurrentFile( _unzFile );
                if (ret != UNZ_OK) {
                    [self OutputErrorMessage:@"file was unzipped but failed crc check"];
                    success = NO;
                }
            }
            
            if (ret == UNZ_OK) {
                ret = unzGoToNextFile( _unzFile );
            }
            
            if (_progressBlock && _numFiles) {
                index++;
                int p = index*100/_numFiles;
                progress = p;
                _progressBlock(progress, index, _numFiles);
            }
        }
	} while (ret==UNZ_OK && ret!=UNZ_END_OF_LIST_OF_FILE);
	return success;
}

-(NSDictionary *)UnzipFileToMemory {
    NSMutableDictionary *fileDictionary = [NSMutableDictionary dictionary];
    
    BOOL success = YES;
    int index = 0;
    int progress = -1;
    int ret = unzGoToFirstFile( _unzFile );
    unsigned char		buffer[4096] = {0};
    if( ret!=UNZ_OK ) {
        [self OutputErrorMessage:@"Failed"];
    }
    
    const char* password = [_password cStringUsingEncoding:NSASCIIStringEncoding];
    
    do{
        @autoreleasepool {
            if( [_password length]==0 )
                ret = unzOpenCurrentFile( _unzFile );
            else
                ret = unzOpenCurrentFilePassword( _unzFile, password );
            if( ret!=UNZ_OK ) {
                [self OutputErrorMessage:@"Error occurs"];
                success = NO;
                break;
            }

            int read ;
            unz_file_info	fileInfo ={0};
            ret = unzGetCurrentFileInfo(_unzFile, &fileInfo, NULL, 0, NULL, 0, NULL, 0);
            if( ret!=UNZ_OK ) {
                [self OutputErrorMessage:@"Error occurs while getting file info"];
                success = NO;
                unzCloseCurrentFile( _unzFile );
                break;
            }
            char* filename = (char*) malloc( fileInfo.size_filename +1 );
            unzGetCurrentFileInfo(_unzFile, &fileInfo, filename, fileInfo.size_filename + 1, NULL, 0, NULL, 0);
            filename[fileInfo.size_filename] = '\0';
            
            NSString * strPath = [NSString stringWithCString:filename encoding:self.stringEncoding];
            free( filename );
            if( [strPath rangeOfCharacterFromSet:[NSCharacterSet characterSetWithCharactersInString:@"/\\"]].location!=NSNotFound ) {
                strPath = [strPath stringByReplacingOccurrencesOfString:@"\\" withString:@"/"];
            }
            
            NSMutableData *fileMutableData = [NSMutableData data];
            do {
                read = unzReadCurrentFile(_unzFile, buffer, 4096);
                if (read >= 0) {
                    if (read != 0) {
                        [fileMutableData appendBytes:buffer length:read];
                    }
                }else{
                    ret = read;
                    success = NO;
                    [self OutputErrorMessage:@"Failed to read zip file"];
                }
            } while (read > 0);
            
            
            if (fileMutableData.length > 0){
                NSData *fileData = [NSData dataWithData:fileMutableData];
                [fileDictionary setObject:fileData forKey:strPath];
            }
            
            if (ret == UNZ_OK) {
                ret = unzCloseCurrentFile( _unzFile );
                if (ret != UNZ_OK) {
                    [self OutputErrorMessage:@"file was unzipped but failed crc check"];
                    success = NO;
                }
            }
            
            if (ret == UNZ_OK) {
                ret = unzGoToNextFile( _unzFile );
            }
            
            if (_progressBlock && _numFiles) {
                index++;
                int p = index*100/_numFiles;
                progress = p;
                _progressBlock(progress, index, _numFiles);
            }
        }
    } while (ret==UNZ_OK && ret!=UNZ_END_OF_LIST_OF_FILE);
    
    NSDictionary *resultDictionary = [NSDictionary dictionaryWithDictionary:fileDictionary];
    return resultDictionary;
}

-(BOOL) UnzipCloseFile {
	self.password = nil;
	if( _unzFile ) {
		int err = unzClose( _unzFile );
        _unzFile = nil;
        return err ==UNZ_OK;
    }
	return YES;
}


-(NSArray*) getZipFileContents {
    int ret = unzGoToFirstFile( _unzFile );
    NSMutableArray * allFilenames = [NSMutableArray arrayWithCapacity:40];
    
    if( ret!=UNZ_OK ) {
        [self OutputErrorMessage:@"Failed"];
    }
    
    const char* password = [_password cStringUsingEncoding:NSASCIIStringEncoding];
    
    do{
        if( [_password length]==0 )
            ret = unzOpenCurrentFile( _unzFile );
        else
            ret = unzOpenCurrentFilePassword( _unzFile, password );
        if( ret!=UNZ_OK ) {
            [self OutputErrorMessage:@"Error occured"];
            break;
        }
        
        unz_file_info   fileInfo ={0};
        ret = unzGetCurrentFileInfo(_unzFile, &fileInfo, NULL, 0, NULL, 0, NULL, 0);
        if( ret!=UNZ_OK ) {
            [self OutputErrorMessage:@"Error occurs while getting file info"];
            unzCloseCurrentFile( _unzFile );
            break;
        }
        char* filename = (char*) malloc( fileInfo.size_filename +1 );
        unzGetCurrentFileInfo(_unzFile, &fileInfo, filename, fileInfo.size_filename + 1, NULL, 0, NULL, 0);
        filename[fileInfo.size_filename] = '\0';
        
        NSString * strPath = [NSString stringWithCString:filename encoding:NSASCIIStringEncoding];
        free( filename );
        if( [strPath rangeOfCharacterFromSet:[NSCharacterSet characterSetWithCharactersInString:@"/\\"]].location!=NSNotFound ) {
            strPath = [strPath stringByReplacingOccurrencesOfString:@"\\" withString:@"/"];
        }
        
        [allFilenames addObject:strPath];
        
        unzCloseCurrentFile( _unzFile );
        ret = unzGoToNextFile( _unzFile );
    }  while( ret==UNZ_OK && UNZ_OK!=UNZ_END_OF_LIST_OF_FILE );
    
    return [NSArray arrayWithArray:allFilenames];
}


#pragma mark wrapper for delegate

-(void) OutputErrorMessage:(NSString*) msg{
	if( _delegate && [_delegate respondsToSelector:@selector(ErrorMessage:)] )
		[_delegate ErrorMessage:msg];
}

-(BOOL) OverWrite:(NSString*) file {
	if( _delegate && [_delegate respondsToSelector:@selector(OverWriteOperation:)] )
		return [_delegate OverWriteOperation:file];
	return YES;
}

#pragma mark get NSDate object for 1980-01-01
-(NSDate*) Date1980 {
	NSDateComponents *comps = [[NSDateComponents alloc] init];
	[comps setDay:1];
	[comps setMonth:1];
	[comps setYear:1980];
	NSCalendar *gregorian = [[NSCalendar alloc]
							 initWithCalendarIdentifier:NSCalendarIdentifierGregorian];
	NSDate *date = [gregorian dateFromComponents:comps];
	
	return date;
}

@end


@implementation NSFileManager(ZipArchive)

- (NSDictionary *)_attributesOfItemAtPath:(NSString *)path followingSymLinks:(BOOL)followingSymLinks error:(NSError **)error {

    NSDictionary* results = [self attributesOfItemAtPath:path error:error];
    if (followingSymLinks && results && (error ? *error == nil : YES)) {
        if ([[results fileType] isEqualToString:NSFileTypeSymbolicLink]) {

            NSString* realPath = [self destinationOfSymbolicLinkAtPath:path error:error];
            if (realPath && (error ? *error == nil : YES)) {
                return [self _attributesOfItemAtPath:realPath followingSymLinks:followingSymLinks error:error];
            } else {

                return nil;
            }
        }
    }
    return results;
}

@end

