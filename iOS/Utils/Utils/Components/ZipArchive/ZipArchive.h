#import <Foundation/Foundation.h>

typedef NS_ENUM(NSInteger, ZipArchiveCompression) {
    ZipArchiveCompressionDefault = -1,
    ZipArchiveCompressionNone    =  0,
    ZipArchiveCompressionSpeed   =  1,
    ZipArchiveCompressionBest    =  9,
};

typedef void(^ZipArchiveProgressUpdateBlock)(int percentage, int filesProcessed, unsigned long numFiles);

@protocol ZipArchiveDelegate <NSObject>
@optional

-(void) ErrorMessage:(NSString*) msg;

-(BOOL) OverWriteOperation:(NSString*) file;

@end

@interface ZipArchive : NSObject {
@private
	void*           _zipFile;
	void*           _unzFile;
	
    unsigned long   _numFiles;
	NSString*       _password;
	id              _delegate;
    ZipArchiveProgressUpdateBlock _progressBlock;
    
    NSArray*    _unzippedFiles;
    
    NSFileManager* _fileManager;
    NSStringEncoding _stringEncoding;
}

@property (nonatomic, retain) id<ZipArchiveDelegate> delegate;
@property (nonatomic, readonly) unsigned long numFiles;
@property (nonatomic, copy) ZipArchiveProgressUpdateBlock progressBlock;

@property (nonatomic, assign) ZipArchiveCompression compression;

@property (nonatomic, assign) NSStringEncoding stringEncoding;

@property (nonatomic, readonly) NSArray* unzippedFiles;

-(id) initWithFileManager:(NSFileManager*) fileManager;

-(BOOL) CreateZipFile2:(NSString*) zipFile;
-(BOOL) CreateZipFile2:(NSString*) zipFile append:(BOOL)isAppend;
-(BOOL) CreateZipFile2:(NSString*) zipFile Password:(NSString*) password;
-(BOOL) CreateZipFile2:(NSString*) zipFile Password:(NSString*) password append:(BOOL)isAppend;
-(BOOL) addFileToZip:(NSString*) file newname:(NSString*) newname;
-(BOOL) addDataToZip:(NSData*) data fileAttributes:(NSDictionary *)attr newname:(NSString*) newname;
-(BOOL) CloseZipFile2;

-(BOOL) UnzipOpenFile:(NSString*) zipFile;
-(BOOL) UnzipOpenFile:(NSString*) zipFile Password:(NSString*) password;
-(BOOL) UnzipFileTo:(NSString*) path overWrite:(BOOL) overwrite;
-(NSDictionary *)UnzipFileToMemory;
-(BOOL) UnzipCloseFile;

-(NSArray*) getZipFileContents;

@end
