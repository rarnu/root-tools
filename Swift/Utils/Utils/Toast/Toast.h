#import <Foundation/Foundation.h>
#import <UIKit/UIKit.h>



typedef enum ToastGravity {
    ToastGravityTop = 0,
    ToastGravityBottom = 1,
    ToastGravityCenter = 2
} ToastGravity;

typedef enum ToastDuration {
    ToastDurationLong = 5000,
    ToastDurationShort = 1000,
    ToastDurationNormal = 3000
} ToastDuration;

typedef enum ToastType {
    ToastTypeInfo = 0,
    ToastTypeNotice = 1,
    ToastTypeWarning = 2,
    ToastTypeError = 3,
    ToastTypeNone = 4
} ToastType;

typedef enum ToastImageLocation {
    ToastImageLocationTop = 0,
    ToastImageLocationLeft = 1
} ToastImageLocation;

@interface ToastSettings : NSObject<NSCopying>

@property enum ToastDuration duration;
@property enum ToastGravity gravity;
@property CGPoint postition;
@property enum ToastType toastType;
@property CGFloat fontSize;
@property BOOL useShadow;
@property CGFloat cornerRadius;
@property CGFloat bgRed;
@property CGFloat bgGreen;
@property CGFloat bgBlue;
@property CGFloat bgAlpha;
@property NSInteger offsetLeft;
@property NSInteger offsetTop;
@property NSDictionary * images;
@property BOOL positionIsSet;
@property enum ToastImageLocation imageLocation;

-(void) setImage:(UIImage *)img forType: (ToastType)type;
-(void) setImage:(UIImage *)img withLocation: (ToastImageLocation)location forType: (ToastType)type;
+(ToastSettings *) getSharedSettings;

@end

@interface Toast : NSObject

@property ToastSettings * settings;
@property NSTimer * timer;
@property UIView * view;
@property NSString * text;

-(id) initWithText:(NSString *) text;
-(void) show;
-(void) show: (ToastType)type;
+(Toast *) makeText: (NSString *)text;

-(Toast *) setDuration: (ToastDuration)duration;
-(Toast *) setGravity: (ToastGravity)gravity offsetLeft: (NSInteger)left offsetTop: (NSInteger)top;
-(Toast *) setGravity: (ToastGravity)gravity;
-(Toast *) setPostion: (CGPoint)position;
-(Toast *) setFontSize: (CGFloat)fontSize;
-(Toast *) setUseShadow: (BOOL)useShadow;
-(Toast *) setCornerRadius: (CGFloat)cornerRadius;
-(Toast *) setBgRed: (CGFloat)bgRed;
-(Toast *) setBgGreen: (CGFloat)bgGreen;
-(Toast *) setBgBlue: (CGFloat)bgBlue;
-(Toast *) setBgAlpha: (CGFloat)bgAlpha;
-(ToastSettings *) theSettings;

@end
