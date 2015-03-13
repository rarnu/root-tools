#import <Foundation/Foundation.h>
#import <CoreGraphics/CoreGraphics.h>
#import "zbar.h"

#ifdef __cplusplus
using namespace zbar;
#endif

@interface ZBarSymbolSet : NSObject <NSFastEnumeration> {
    const zbar_symbol_set_t *set;
    BOOL filterSymbols;
}

@property (readonly, nonatomic) int count;
@property (readonly, nonatomic) const zbar_symbol_set_t *zbarSymbolSet;
@property (nonatomic) BOOL filterSymbols;

- (id) initWithSymbolSet: (const zbar_symbol_set_t*) set;

@end


@interface ZBarSymbol : NSObject {
    const zbar_symbol_t *symbol;
}

@property (readonly, nonatomic) zbar_symbol_type_t type;
@property (readonly, nonatomic) NSString *typeName;
@property (readonly, nonatomic) NSUInteger configMask;
@property (readonly, nonatomic) NSUInteger modifierMask;
@property (readonly, nonatomic) NSString *data;
@property (readonly, nonatomic) int quality;
@property (readonly, nonatomic) int count;
@property (readonly, nonatomic) zbar_orientation_t orientation;
@property (readonly, nonatomic) ZBarSymbolSet *components;
@property (readonly, nonatomic) const zbar_symbol_t *zbarSymbol;
@property (readonly, nonatomic) CGRect bounds;

- (id) initWithSymbol: (const zbar_symbol_t*) symbol;

+ (NSString*) nameForType: (zbar_symbol_type_t) type;

@end
