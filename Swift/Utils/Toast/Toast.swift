import UIKit

var CURRENT_TOAST_TAG = 6984678
let kComponentPadding: CGFloat = 5

enum ToastGravity {
    case Top, Bottom, Center
    func toInt() -> Int {
        switch(self) {
        case .Top:
            return 0
        case .Bottom:
            return 1
        case .Center:
            return 2
        }
    }
}

enum ToastDuration {
    case Long, Short, Normal
    func toInt() -> Int {
        switch(self){
        case .Long:
            return 5000
        case .Short:
            return 1000
        case .Normal:
            return 3000
        }
    }
}

enum ToastType {
    case Info, Notice, Warning, Error, None
    func toInt() -> Int {
        switch(self) {
        case .Info:
            return 0
        case .Notice:
            return 1
        case .Warning:
            return 2
        case .Error:
            return 3
        case .None:
            return 4
        }
    }
    static func fromInt(i: Int) -> ToastType {
        switch(i) {
        case 0:
            return ToastType.Info
        case 1:
            return ToastType.Notice
        case 2:
            return ToastType.Warning
        case 3:
            return ToastType.Error
        default:
            return ToastType.None
        }
    }
}

enum ToastImageLocation {
    case Top, Left
    func toInt() -> Int {
        switch(self) {
        case .Top:
            return 0
        case .Left:
            return 1
        }
    }
}

var sharedSettings: NSObject?

class ToastSettings : NSObject, NSCopying {

    var duration: ToastDuration?
    var gravity: ToastGravity?
    var postition: CGPoint?
    var toastType: ToastType?
    var fontSize: CGFloat?
    var useShadow: Bool?
    var cornerRadius: CGFloat?
    var bgRed: CGFloat?
    var bgGreen: CGFloat?
    var bgBlue: CGFloat?
    var bgAlpha: CGFloat?
    var offsetLeft: NSInteger?
    var offsetTop: NSInteger?
    var images: NSDictionary?
    var positionIsSet: Bool?
    var imageLocation: ToastImageLocation?
    
    func setImage(img: UIImage?, forType type: ToastType) {
        setImage(img, withLocation: ToastImageLocation.Left, forType: type)
    }

    func setImage(img: UIImage?, withLocation location: ToastImageLocation, forType type: ToastType) {
        if (type == ToastType.None) {
            return
        }
        if (images == nil) {
            images = NSMutableDictionary(capacity: 4)
        }
        if (img != nil) {
            // NSString *key = [NSString stringWithFormat:@"%i", type];
            var key = "\(type.toInt())"
            NSLog(key)
            images!.setValue(img, forKey: key)
        }
        imageLocation = location
    }

    class func getSharedSettings() -> ToastSettings? {
        if (sharedSettings == nil) {
            sharedSettings = ToastSettings()
            (sharedSettings as ToastSettings).gravity = ToastGravity.Center
            (sharedSettings as ToastSettings).duration = ToastDuration.Short
            (sharedSettings as ToastSettings).fontSize = 16.0;
            (sharedSettings as ToastSettings).useShadow = true;
            (sharedSettings as ToastSettings).cornerRadius = 5.0;
            (sharedSettings as ToastSettings).bgRed = 0;
            (sharedSettings as ToastSettings).bgGreen = 0;
            (sharedSettings as ToastSettings).bgBlue = 0;
            (sharedSettings as ToastSettings).bgAlpha = 0.7;
            (sharedSettings as ToastSettings).offsetLeft = 0;
            (sharedSettings as ToastSettings).offsetTop = 0;
        }
        return (sharedSettings as ToastSettings)
    }
    
    func copyWithZone(zone: NSZone) -> AnyObject {
        var copy = ToastSettings()
        
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
    
        var keys = self.images!.allKeys as NSArray
        for key in keys {
            copy.setImage(images!.valueForKey(key as String) as? UIImage, forType: ToastType.fromInt(key as Int))
        }
        copy.imageLocation = imageLocation
        return copy
    }
}

class Toast: NSObject {
    var _settings: ToastSettings?
    var timer: NSTimer?
    var view: UIView?
    var text: NSString?
    
    init(txt: NSString) {
        text = txt
    }
    
    func show() {
        show(ToastType.None)
    }
    
    func show(type: ToastType) {
        var theSettings = _settings
        
        if (theSettings == nil) {
            theSettings = ToastSettings.getSharedSettings()
        }
        
        var image: UIImage? = theSettings!.images?.valueForKey(String(type.toInt())) as? UIImage
        var font = UIFont.systemFontOfSize(theSettings!.fontSize!)
        
        var textSize = text!.sizeWithAttributes([NSFontAttributeName: font])
        
        var label = UILabel(frame: CGRectMake(0, 0, textSize.width + kComponentPadding, textSize.height + kComponentPadding))
        
        label.backgroundColor = UIColor.clearColor()
        label.textColor = UIColor.whiteColor()
        label.font = font
        label.text = text
        label.numberOfLines = 0
        label.textAlignment = NSTextAlignment.Center
        
        if (theSettings!.useShadow!) {
            label.shadowColor = UIColor.darkGrayColor()
            label.shadowOffset = CGSizeMake(1, 1)
        }
        
        var v = UIButton.buttonWithType(UIButtonType.Custom) as UIButton
        if (image != nil) {
            v.frame = _toastFrameForImageSize(image!.size, withLocation: theSettings!.imageLocation!, andTextSize: textSize)
            switch (theSettings!.imageLocation!) {
            case ToastImageLocation.Left:
                label.center = CGPointMake(image!.size.width + kComponentPadding * 2
                    + (v.frame.size.width - image!.size.width - kComponentPadding * 2) / 2,
                    v.frame.size.height / 2)
            case ToastImageLocation.Top:
                label.center = CGPointMake(v.frame.size.width / 2,
                    (image!.size.height + kComponentPadding * 2
                        + (v.frame.size.height - image!.size.height - kComponentPadding * 2) / 2))
            }
            
        } else {
            v.frame = CGRectMake(0, 0, textSize.width + kComponentPadding * 2, textSize.height + kComponentPadding * 2)
            label.center = CGPointMake(v.frame.size.width / 2, v.frame.size.height / 2)
        }
        var lbfrm = label.frame
        lbfrm.origin.x = ceil(lbfrm.origin.x)
        lbfrm.origin.y = ceil(lbfrm.origin.y)
        label.frame = lbfrm
        v.addSubview(label)
        if (image != nil) {
            var imageView = UIImageView(image: image)
            imageView.frame = _frameForImage(type, inToastFrame: v.frame)
            v.addSubview(imageView)
        }
        
        v.backgroundColor = UIColor(red: theSettings!.bgRed!, green: theSettings!.bgGreen!, blue: theSettings!.bgBlue!, alpha: theSettings!.bgAlpha!)
        v.layer.cornerRadius = theSettings!.cornerRadius!
        
        var window: UIWindow? = UIApplication.sharedApplication().windows[0] as? UIWindow
        
        var point: CGPoint?
        
        // Set correct orientation/location regarding device orientation
        var orientation = UIApplication.sharedApplication().statusBarOrientation
        
        switch (orientation) {
        case UIInterfaceOrientation.Portrait:
                if (theSettings!.gravity! == ToastGravity.Top) {
                    point = CGPointMake(window!.frame.size.width / 2, 45)
                } else if (theSettings!.gravity! == ToastGravity.Bottom) {
                    point = CGPointMake(window!.frame.size.width / 2, window!.frame.size.height - 45)
                } else if (theSettings!.gravity! == ToastGravity.Center) {
                    point = CGPointMake(window!.frame.size.width/2, window!.frame.size.height/2)
                } else {
                    point = theSettings!.postition!
                }
                point = CGPointMake(point!.x + CGFloat(theSettings!.offsetLeft!), point!.y + CGFloat(theSettings!.offsetTop!))
            
        case UIInterfaceOrientation.PortraitUpsideDown:
                v.transform = CGAffineTransformMakeRotation(CGFloat(M_PI))
                
                var width = window!.frame.size.width
                var height = window!.frame.size.height
                
                if (theSettings!.gravity! == ToastGravity.Top) {
                    point = CGPointMake(width / 2, height - 45)
                } else if (theSettings!.gravity! == ToastGravity.Bottom) {
                    point = CGPointMake(width / 2, 45)
                } else if (theSettings!.gravity! == ToastGravity.Center) {
                    point = CGPointMake(width/2, height/2)
                } else {
                    point = theSettings!.postition!
                }
                
                point = CGPointMake(point!.x - CGFloat(theSettings!.offsetLeft!), point!.y - CGFloat(theSettings!.offsetTop!))
            
        case UIInterfaceOrientation.LandscapeLeft:
                v.transform = CGAffineTransformMakeRotation(CGFloat(M_PI/2))
                
                if (theSettings!.gravity! == ToastGravity.Top) {
                    point = CGPointMake(window!.frame.size.width - 45, window!.frame.size.height / 2)
                } else if (theSettings!.gravity! == ToastGravity.Bottom) {
                    point = CGPointMake(45,window!.frame.size.height / 2)
                } else if (theSettings!.gravity! == ToastGravity.Center) {
                    point = CGPointMake(window!.frame.size.width/2, window!.frame.size.height/2)
                } else {
                    point = theSettings!.postition!
                }
                
                point = CGPointMake(point!.x - CGFloat(theSettings!.offsetTop!), point!.y - CGFloat(theSettings!.offsetLeft!))
            
        case UIInterfaceOrientation.LandscapeRight:
                v.transform = CGAffineTransformMakeRotation(CGFloat(-M_PI/2))
                
                if (theSettings!.gravity! == ToastGravity.Top) {
                    point = CGPointMake(45, window!.frame.size.height / 2)
                } else if (theSettings!.gravity! == ToastGravity.Bottom) {
                    point = CGPointMake(window!.frame.size.width - 45, window!.frame.size.height/2)
                } else if (theSettings!.gravity! == ToastGravity.Center) {
                    point = CGPointMake(window!.frame.size.width/2, window!.frame.size.height/2)
                } else {
                    point = theSettings!.postition!
                }
                
                point = CGPointMake(point!.x + CGFloat(theSettings!.offsetTop!), point!.y + CGFloat(theSettings!.offsetLeft!))
        default:
            break
        }
        
        v.center = point!
        v.frame = CGRectIntegral(v.frame);
        // theSettings!.duration / 1000
        var interval = Double(theSettings!.duration!.toInt() / 1000)
        var timer1 = NSTimer(timeInterval: interval, target: self, selector: "hideToast:", userInfo: nil, repeats: false)
        NSRunLoop.mainRunLoop().addTimer(timer1, forMode: NSDefaultRunLoopMode)
        
        v.tag = CURRENT_TOAST_TAG
        
        var currentToast = window?.viewWithTag(CURRENT_TOAST_TAG)
        if (currentToast != nil) {
            currentToast?.removeFromSuperview()
        }
        
        v.alpha = 0
        window?.addSubview(v)
        UIView.beginAnimations(nil, context: nil)
        v.alpha = 1
        UIView.commitAnimations()
        view = v
        v.addTarget(self, action: "hideToast:", forControlEvents: UIControlEvents.TouchDown)

    }
    
    class func makeText(text: NSString) -> Toast {
        var toast = Toast(txt: text)
        return toast
    }
    
    func _toastFrameForImageSize(imageSize: CGSize, withLocation location: ToastImageLocation, andTextSize textSize: CGSize) -> CGRect {
        var theRect = CGRectZero
        switch (location) {
        case ToastImageLocation.Left:
            theRect = CGRectMake(0, 0,
                imageSize.width + textSize.width + kComponentPadding * 3,
                max(textSize.height, imageSize.height) + kComponentPadding * 2);
        case ToastImageLocation.Top:
            theRect = CGRectMake(0, 0,
                max(textSize.width, imageSize.width) + kComponentPadding * 2,
                imageSize.height + textSize.height + kComponentPadding * 3);
        }
        return theRect
    }
    
    func _frameForImage(type: ToastType, inToastFrame toastFrame: CGRect) -> CGRect {
        var theSettings = _settings
        var image: UIImage? = theSettings!.images?.valueForKey(String(type.toInt())) as? UIImage
        if (image == nil) {
            return CGRectZero
        }
        var imageFrame = CGRectZero
        switch (theSettings!.imageLocation!) {
        case ToastImageLocation.Left:
            imageFrame = CGRectMake(kComponentPadding, (toastFrame.size.height - image!.size.height) / 2, image!.size.width, image!.size.height);
        case ToastImageLocation.Top:
            imageFrame = CGRectMake((toastFrame.size.width - image!.size.width) / 2, kComponentPadding, image!.size.width, image!.size.height);
        }
        return imageFrame
    }
    
    
    func hideToast(theTimer: NSTimer) {
        UIView.beginAnimations(nil, context: nil)
        view?.alpha = 0
        UIView.commitAnimations()
        
        var timer2 = NSTimer(timeInterval: 500, target: self, selector: "hideToast:", userInfo: nil, repeats: false)
        NSRunLoop.mainRunLoop().addTimer(timer2, forMode: NSDefaultRunLoopMode)
    }
    
    func removeToast(theTimer: NSTimer) {
        view?.removeFromSuperview()
    }
    
    func setDuration(duration: ToastDuration) -> Toast {
        self.theSettings()?.duration = duration
        return self
    }
    func setGravity(gravity: ToastGravity, offsetLeft left: NSInteger, offsetTop top: NSInteger) -> Toast {
        self.theSettings()?.gravity = gravity
        self.theSettings()?.offsetLeft = left
        self.theSettings()?.offsetTop = top
        return self
    }
    func setGravity(gravity: ToastGravity) -> Toast {
        self.theSettings()?.gravity = gravity
        return self
    }
    func setPostion(position: CGPoint) -> Toast {
        self.theSettings()?.postition = CGPointMake(position.x, position.y);
        return self
    }
    func setFontSize(fontSize: CGFloat) -> Toast {
        self.theSettings()?.fontSize = fontSize
        return self
    }
    func setUseShadow(useShadow: Bool) -> Toast {
        self.theSettings()?.useShadow = useShadow
        return self
    }
    func setCornerRadius(cornerRadius: CGFloat) -> Toast {
        self.theSettings()?.cornerRadius = cornerRadius
        return self
    }
    func setBgRed(bgRed: CGFloat) -> Toast {
        self.theSettings()?.bgRed = bgRed
        return self
    }
    func setBgGreen(bgGreen: CGFloat) -> Toast {
        self.theSettings()?.bgGreen = bgGreen
        return self
    }
    func setBgBlue(bgBlue: CGFloat) -> Toast {
        self.theSettings()?.bgBlue = bgBlue
        return self
    }
    func setBgAlpha(bgAlpha: CGFloat) -> Toast {
        self.theSettings()?.bgAlpha = bgAlpha
        return self
    }
    
    func theSettings() -> ToastSettings? {
        if (_settings == nil) {
            _settings = ToastSettings.getSharedSettings()
        }
        return _settings
    }
    
}
