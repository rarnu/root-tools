#import "Toast.h"

#define CURRENT_TOAST_TAG 6984678

static ToastSettings * sharedSettings = nil;
static CGFloat kComponentPadding = 5.0;

@implementation ToastSettings

-(void) setImage:(UIImage *)img forType: (ToastType)type {
    [self setImage:img withLocation:ToastImageLocationLeft forType:type];
}

-(void) setImage:(UIImage *)img withLocation: (ToastImageLocation)location forType: (ToastType)type {
    if (type == ToastTypeNone) {
        return;
    }
    if (self.images == nil) {
        self.images = [NSMutableDictionary dictionaryWithCapacity:4];
    }
    if (img != nil) {
        NSString *key = [NSString stringWithFormat:@"%i", type];
        [self.images setValue:img forKey:key];
    }
    self.imageLocation = location;
}

+(ToastSettings *) getSharedSettings {
    if (sharedSettings == nil) {
        sharedSettings = [[ToastSettings alloc] init];
        sharedSettings.gravity = ToastGravityCenter;
        sharedSettings.duration = ToastDurationShort;
        sharedSettings.fontSize = 16.0;
        sharedSettings.useShadow = YES;
        sharedSettings.cornerRadius = 5.0;
        sharedSettings.bgRed = 0;
        sharedSettings.bgGreen = 0;
        sharedSettings.bgBlue = 0;
        sharedSettings.bgAlpha = 0.7;
        sharedSettings.offsetLeft = 0;
        sharedSettings.offsetTop = 0;
    }
    return sharedSettings;
}

-(id) copyWithZone: (NSZone *)zone {
    ToastSettings * copy = [[ToastSettings alloc] init];
    copy.gravity = self.gravity;
    copy.duration = self.duration;
    copy.postition = self.postition;
    copy.fontSize = self.fontSize;
    copy.useShadow = self.useShadow;
    copy.cornerRadius = self.cornerRadius;
    copy.bgRed = self.bgRed;
    copy.bgGreen = self.bgGreen;
    copy.bgBlue = self.bgBlue;
    copy.bgAlpha = self.bgAlpha;
    copy.offsetLeft = self.offsetLeft;
    copy.offsetTop = self.offsetTop;
    
    NSArray * keys = self.images.allKeys;
    for (NSString * key in keys) {
        [copy setImage:(UIImage *)[self.images valueForKey:key] forType:[key intValue]];
    }
    copy.imageLocation = self.imageLocation;
    return copy;
}

@end


@implementation Toast

-(id) initWithText:(NSString *) text {
    if (self = [super init]) {
        self.text = [text copy];
    }
    return self;
}

-(void) show {
    [self show:ToastTypeNone];
}

-(void) show: (ToastType)type {
    
    ToastSettings * theSettings = self.settings;
    if (theSettings == nil) {
        theSettings = [ToastSettings getSharedSettings];
    }
    
    UIImage * image = [theSettings.images valueForKey:[NSString stringWithFormat:@"%d", type]];
    UIFont * font = [UIFont systemFontOfSize:theSettings.fontSize];
    CGSize textSize = [self.text sizeWithAttributes:[NSDictionary dictionaryWithObjectsAndKeys:font, NSFontAttributeName, nil]];
    
    UILabel * label = [[UILabel alloc] initWithFrame: CGRectMake(0, 0, textSize.width + kComponentPadding, textSize.height + kComponentPadding)];
    
    label.backgroundColor = [UIColor clearColor];
    label.textColor = [UIColor whiteColor];
    label.font = font;
    label.text = self.text;
    label.numberOfLines = 0;
    label.textAlignment = NSTextAlignmentCenter;
    
    if (theSettings.useShadow) {
        label.shadowColor = [UIColor darkGrayColor];
        label.shadowOffset = CGSizeMake(1, 1);
    }
    
    UIButton * v = [UIButton buttonWithType:UIButtonTypeCustom];
    if (image != nil) {
        v.frame = [self _toastFrameForImageSize:image.size withLocation:theSettings.imageLocation andTextSize:textSize];
        switch (theSettings.imageLocation) {
            case ToastImageLocationLeft:
                label.center = CGPointMake(image.size.width + kComponentPadding * 2 + (v.frame.size.width - image.size.width - kComponentPadding * 2) / 2, v.frame.size.height / 2);
                break;
            case ToastImageLocationTop:
                label.center = CGPointMake(v.frame.size.width / 2, (image.size.height + kComponentPadding * 2 + (v.frame.size.height - image.size.height - kComponentPadding * 2) / 2));
                break;
        }
    } else {
        v.frame = CGRectMake(0, 0, textSize.width + kComponentPadding * 2, textSize.height + kComponentPadding * 2);
        label.center = CGPointMake(v.frame.size.width / 2, v.frame.size.height / 2);
    }
    CGRect lbfrm = label.frame;
    lbfrm.origin.x = ceil(lbfrm.origin.x);
    lbfrm.origin.y = ceil(lbfrm.origin.y);
    label.frame = lbfrm;
    [v addSubview:label];

    if (image != nil) {
        UIImageView * imageView = [[UIImageView alloc] initWithImage:image];
        imageView.frame = [self _frameForImage:type inToastFrame: v.frame];
        [v addSubview:imageView];
    }
    
    v.backgroundColor = [UIColor colorWithRed:theSettings.bgRed green:theSettings.bgGreen blue:theSettings.bgBlue alpha:theSettings.bgAlpha];
    v.layer.cornerRadius = theSettings.cornerRadius;
    
    UIWindow * window = [UIApplication sharedApplication].windows[0];
    CGPoint point;
    UIInterfaceOrientation orientation = [UIApplication sharedApplication].statusBarOrientation;
    
    switch (orientation) {
        case UIInterfaceOrientationPortrait:
            if (theSettings.gravity == ToastGravityTop) {
                point = CGPointMake(window.frame.size.width / 2, 45);
            } else if (theSettings.gravity == ToastGravityBottom) {
                point = CGPointMake(window.frame.size.width / 2, window.frame.size.height - 45);
            } else if (theSettings.gravity == ToastGravityCenter) {
                point = CGPointMake(window.frame.size.width/2, window.frame.size.height/2);
            } else {
                point = theSettings.postition;
            }
            point = CGPointMake(point.x + theSettings.offsetLeft, point.y + theSettings.offsetTop);
            break;
            
        case UIInterfaceOrientationPortraitUpsideDown:
            v.transform = CGAffineTransformMakeRotation(M_PI);
            CGFloat width = window.frame.size.width;
            CGFloat height = window.frame.size.height;
            
            if (theSettings.gravity == ToastGravityTop) {
                point = CGPointMake(width / 2, height - 45);
            } else if (theSettings.gravity == ToastGravityBottom) {
                point = CGPointMake(width / 2, 45);
            } else if (theSettings.gravity == ToastGravityCenter) {
                point = CGPointMake(width/2, height/2);
            } else {
                point = theSettings.postition;
            }
            
            point = CGPointMake(point.x - theSettings.offsetLeft, point.y - theSettings.offsetTop);
            break;
        case UIInterfaceOrientationLandscapeLeft:
            v.transform = CGAffineTransformMakeRotation(M_PI/2);
            if (theSettings.gravity == ToastGravityTop) {
                point = CGPointMake(window.frame.size.width - 45, window.frame.size.height / 2);
            } else if (theSettings.gravity == ToastGravityBottom) {
                point = CGPointMake(45, window.frame.size.height / 2);
            } else if (theSettings.gravity == ToastGravityCenter) {
                point = CGPointMake(window.frame.size.width/2, window.frame.size.height/2);
            } else {
                point = theSettings.postition;
            }
            
            point = CGPointMake(point.x - theSettings.offsetTop, point.y - theSettings.offsetLeft);
            break;
        case UIInterfaceOrientationLandscapeRight:
            v.transform = CGAffineTransformMakeRotation(-M_PI/2);
            
            if (theSettings.gravity == ToastGravityTop) {
                point = CGPointMake(45, window.frame.size.height / 2);
            } else if (theSettings.gravity == ToastGravityBottom) {
                point = CGPointMake(window.frame.size.width - 45, window.frame.size.height/2);
            } else if (theSettings.gravity == ToastGravityCenter) {
                point = CGPointMake(window.frame.size.width/2, window.frame.size.height/2);
            } else {
                point = theSettings.postition;
            }
            
            point = CGPointMake(point.x + theSettings.offsetTop, point.y + theSettings.offsetLeft);
            break;
        default:
            break;
    }
    
    v.center = point;
    v.frame = CGRectIntegral(v.frame);

    double interval = theSettings.duration / 1000;
    NSTimer * timer1 = [NSTimer timerWithTimeInterval:interval target:self selector:@selector(hideToast:) userInfo:nil repeats:NO];
    [[NSRunLoop mainRunLoop] addTimer:timer1 forMode:NSDefaultRunLoopMode];
    
    v.tag = CURRENT_TOAST_TAG;
    UIView * currentToast = [window viewWithTag: CURRENT_TOAST_TAG];
    if (currentToast != nil) {
        [currentToast removeFromSuperview];
    }
    v.alpha = 0;
    [window addSubview: v];
    [UIView beginAnimations:nil context:nil];
    v.alpha = 1;
    [UIView commitAnimations];
    self.view = v;
    [v addTarget:self action:@selector(hideToast:) forControlEvents:UIControlEventTouchDown];
    
}

+(Toast *) makeText: (NSString *)text {
    Toast * toast = [[Toast alloc] initWithText:text];
    return toast;
}

-(CGRect) _toastFrameForImageSize: (CGSize)imageSize withLocation: (ToastImageLocation)location andTextSize: (CGSize)textSize {
    CGRect theRect = CGRectZero;
    switch (location) {
        case ToastImageLocationLeft:
            theRect = CGRectMake(0, 0, imageSize.width + textSize.width + kComponentPadding * 3, MAX(textSize.height, imageSize.height) + kComponentPadding * 2);
            break;
        case ToastImageLocationTop:
            theRect = CGRectMake(0, 0, MAX(textSize.width, imageSize.width) + kComponentPadding * 2, imageSize.height + textSize.height + kComponentPadding * 3);
            break;
    }
    return theRect;
}

-(CGRect) _frameForImage: (ToastType)type inToastFrame: (CGRect)toastFrame {
    ToastSettings * theSettings = self.settings;
    UIImage * image = [theSettings.images valueForKey:[NSString stringWithFormat:@"%d", type]];
    if (image == nil) {
        return CGRectZero;
    }
    CGRect imageFrame = CGRectZero;
    switch (theSettings.imageLocation) {
        case ToastImageLocationLeft:
            imageFrame = CGRectMake(kComponentPadding, (toastFrame.size.height - image.size.height) / 2, image.size.width, image.size.height);
            break;
        case ToastImageLocationTop:
            imageFrame = CGRectMake((toastFrame.size.width - image.size.width) / 2, kComponentPadding, image.size.width, image.size.height);
            break;
    }
    return imageFrame;
}


-(void) hideToast: (NSTimer *)theTimer {
    [UIView beginAnimations:nil context:nil];
    self.view.alpha = 0;
    [UIView commitAnimations];
    NSTimer * timer2 = [NSTimer timerWithTimeInterval:500 target:self selector:@selector(hideToast:) userInfo:nil repeats:NO];
    [[NSRunLoop mainRunLoop] addTimer:timer2 forMode:NSDefaultRunLoopMode];
}

-(void) removeToast: (NSTimer *)theTimer {
    [self.view removeFromSuperview];
}

-(Toast *) setDuration: (ToastDuration)duration {
    [self theSettings].duration = duration;
    return self;
}

-(Toast *) setGravity: (ToastGravity)gravity offsetLeft: (NSInteger)left offsetTop: (NSInteger)top {
    [self theSettings].gravity = gravity;
    [self theSettings].offsetLeft = left;
    [self theSettings].offsetTop = top;
    return self;
}
-(Toast *) setGravity: (ToastGravity)gravity {
    [self theSettings].gravity = gravity;
    return self;
}
-(Toast *) setPostion: (CGPoint)position {
    [self theSettings].postition = CGPointMake(position.x, position.y);
    return self;
}
-(Toast *) setFontSize: (CGFloat)fontSize {
    [self theSettings].fontSize = fontSize;
    return self;
}
-(Toast *) setUseShadow: (BOOL)useShadow {
    [self theSettings].useShadow = useShadow;
    return self;
}
-(Toast *) setCornerRadius: (CGFloat)cornerRadius {
    [self theSettings].cornerRadius = cornerRadius;
    return self;
}
-(Toast *) setBgRed: (CGFloat)bgRed {
    [self theSettings].bgRed = bgRed;
    return self;
}
-(Toast *) setBgGreen: (CGFloat)bgGreen {
    [self theSettings].bgGreen = bgGreen;
    return self;
}

-(Toast *) setBgBlue: (CGFloat)bgBlue {
    [self theSettings].bgBlue = bgBlue;
    return self;
}

-(Toast *) setBgAlpha: (CGFloat)bgAlpha {
    [self theSettings].bgAlpha = bgAlpha;
    return self;
}

-(ToastSettings *) theSettings {
    if (self.settings == nil) {
        self.settings = [ToastSettings getSharedSettings];
    }
    return self.settings;
}

@end
