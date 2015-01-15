import UIKit

@objc
protocol SideMenuDelegate: NSObjectProtocol {
    optional func sideMenu(sideMenu:SideMenu, didRecognizePanGesture recognizer: UIPanGestureRecognizer?)
    optional func sideMenu(sideMenu:SideMenu, willShowMenuViewController menuViewController: UIViewController?)
    optional func sideMenu(sideMenu:SideMenu, didShowMenuViewController menuViewController: UIViewController?)
    optional func sideMenu(sideMenu:SideMenu, willHideMenuViewController menuViewController: UIViewController?)
    optional func sideMenu(sideMenu:SideMenu, didHideMenuViewController menuViewController: UIViewController?)
    
}

class SideMenu: UIViewController, UIGestureRecognizerDelegate {

    var _contentViewController: UIViewController? = nil
    
    var contentViewController: UIViewController? {
        get {
            return self._contentViewController
        }
        set (controller) {
            if (self._contentViewController == nil) {
                self._contentViewController = controller
                return
            }
            self.__hideViewController(self._contentViewController)
            self._contentViewController = controller
            self.addChildViewController(self._contentViewController!)
            self._contentViewController!.view.frame = self.view.bounds
            self.contentViewContainer!.addSubview(self._contentViewController!.view)
            self._contentViewController!.didMoveToParentViewController(self)
            
            self.__updateContentViewShadow()
            
            if (self.visible!) {
                self.__addContentViewControllerMotionEffects()
            }

        }
    }
    
    var _leftMenuViewController: UIViewController?
    var leftMenuViewController: UIViewController? {
        get {
            return self._leftMenuViewController
        }
        set (controller) {
            if (self._leftMenuViewController == nil) {
                self._leftMenuViewController = controller
                return
            }
            self.__hideViewController(self._leftMenuViewController)
            self._leftMenuViewController = controller
            
            self.addChildViewController(self._leftMenuViewController!)
            self._leftMenuViewController!.view.frame = self.view.bounds
            self._leftMenuViewController!.view.autoresizingMask = UIViewAutoresizing.FlexibleWidth | UIViewAutoresizing.FlexibleHeight;
            self.menuViewContainer!.addSubview(self._leftMenuViewController!.view)
            self._leftMenuViewController!.didMoveToParentViewController(self)
            
            self.__addMenuViewControllerMotionEffects()
            self.view.bringSubviewToFront(self.contentViewContainer!)
        }
    }
    
    var _rightMenuViewController: UIViewController?
    var rightMenuViewController: UIViewController? {
        get {
            return self._rightMenuViewController
        }
        set (controller) {
            if (self._rightMenuViewController == nil) {
                self._rightMenuViewController = controller
                return
            }
            self.__hideViewController(self._rightMenuViewController)
            self._rightMenuViewController = controller
            
            self.addChildViewController(self._rightMenuViewController!)
            self._rightMenuViewController!.view.frame = self.view.bounds
            self._rightMenuViewController!.view.autoresizingMask = UIViewAutoresizing.FlexibleWidth | UIViewAutoresizing.FlexibleHeight;
            self.menuViewContainer!.addSubview(self._rightMenuViewController!.view)
            self._rightMenuViewController!.didMoveToParentViewController(self)
            
            self.__addMenuViewControllerMotionEffects()
            self.view.bringSubviewToFront(self.contentViewContainer!)

        }
    }
    var delegate: SideMenuDelegate?
    var animationDuration: NSTimeInterval?
    
    var _backgroundImage: UIImage?
    var backgroundImage: UIImage? {
        get {
            return self._backgroundImage
        }
        set (bgImg) {
            self._backgroundImage = bgImg;
            if (self.backgroundImageView != nil) {
                self.backgroundImageView!.image = _backgroundImage!
            }
        }
    }
    var panGestureEnabled: Bool?
    var panFromEdge: Bool?
    var panMinimumOpenThreshold: Int?
    var panMinimumEdgeSize: CGFloat?
    var interactivePopGestureRecognizerEnabled: Bool?
    var fadeMenuView: Bool?
    var scaleContentView: Bool?
    var scaleBackgroundImageView: Bool?
    var scaleMenuView: Bool?
    var contentViewShadowEnabled: Bool?
    var contentViewShadowColor: UIColor?
    var contentViewShadowOffset: CGSize?
    var contentViewShadowOpacity: CGFloat?
    var contentViewShadowRadius: CGFloat?
    var contentViewScaleValue: CGFloat?
    var contentViewInLandscapeOffsetCenterX: CGFloat?
    var contentViewInPortraitOffsetCenterX: CGFloat?
    var parallaxMenuMinimumRelativeValue: CGFloat?
    var parallaxMenuMaximumRelativeValue: CGFloat?
    var parallaxContentMinimumRelativeValue: CGFloat?
    var parallaxContentMaximumRelativeValue: CGFloat?
    var menuViewControllerTransformation: CGAffineTransform?
    var parallaxEnabled: Bool?
    var bouncesHorizontally: Bool?
    var menuPreferredStatusBarStyle: UIStatusBarStyle?
    var menuPrefersStatusBarHidden: Bool? = false

    var backgroundImageView: UIImageView?
    var visible: Bool? = false
    var leftMenuVisible: Bool? = false
    var rightMenuVisible: Bool? = false
    var originalPoint: CGPoint?
    var contentButton: UIButton?
    var menuViewContainer: UIView?
    var contentViewContainer: UIView?
    var didNotifyDelegate: Bool?
    
    required override init() {
        super.init()
        self.__commonInit()
    }
    required init(coder aDecoder: NSCoder) {
        super.init(coder: aDecoder)
        self.__commonInit()
    }
    
    func __commonInit() {
        self.menuViewContainer = UIView()
        self.contentViewContainer = UIView()
        self.animationDuration = 0.35
        self.interactivePopGestureRecognizerEnabled = true
        self.menuViewControllerTransformation = CGAffineTransformMakeScale(1.5, 1.5)
        self.scaleContentView = true
        self.scaleBackgroundImageView = true
        self.scaleMenuView = true
        self.fadeMenuView = true
        self.parallaxEnabled = true
        self.parallaxMenuMinimumRelativeValue = -15
        self.parallaxMenuMaximumRelativeValue = 15
        self.parallaxContentMinimumRelativeValue = -25
        self.parallaxContentMaximumRelativeValue = 25
        self.bouncesHorizontally = true
        self.panGestureEnabled = true
        self.panFromEdge = true
        self.panMinimumOpenThreshold = 60
        self.panMinimumEdgeSize = 50
        self.contentViewShadowEnabled = false
        self.contentViewShadowColor = UIColor.blackColor()
        self.contentViewShadowOffset = CGSizeZero
        self.contentViewShadowOpacity = 0.4
        self.contentViewShadowRadius = 8.0
        self.contentViewInLandscapeOffsetCenterX = 30.0
        self.contentViewInPortraitOffsetCenterX  = 30.0
        self.contentViewScaleValue = 0.9
    }

    // =================================
    // public methods
    // =================================
    
    init(contentViewController:UIViewController, leftMenuViewController: UIViewController, rightMenuViewController: UIViewController) {
        super.init()
        self.contentViewController = contentViewController
        self.leftMenuViewController = leftMenuViewController
        self.rightMenuViewController = rightMenuViewController
    }
    
    func presentLeftMenuViewController() {
        self.__presentMenuViewContainerWithMenuViewController(self.leftMenuViewController)
        self.__showLeftMenuViewController()
    }
    
    func presentRightMenuViewController() {
        self.__presentMenuViewContainerWithMenuViewController(self.rightMenuViewController)
        self.__showRightMenuViewController()
    }
    
    func hideMenuViewController() {
        self.__hideMenuViewControllerAnimated(true)
    }
    
    func setContentViewController(contentViewController:UIViewController, animated:Bool) {
        if (self.contentViewController == contentViewController) {
            return
        }
    
        if (!animated) {
            self.contentViewController = contentViewController
        } else {
            self.addChildViewController(contentViewController)
            contentViewController.view.alpha = 0;
            contentViewController.view.frame = self.contentViewContainer!.bounds
            self.contentViewContainer!.addSubview(contentViewController.view)
            func _anim_doing() {
                contentViewController.view.alpha = 1
            }
            func _anim_complete(var finish: Bool) {
                self.__hideViewController(self.contentViewController)
                contentViewController.didMoveToParentViewController(self)
                self.contentViewController = contentViewController
                self.__statusBarNeedsAppearanceUpdate()
                self.__updateContentViewShadow()
                if (self.visible!) {
                    self.__addContentViewControllerMotionEffects()
                }

            }
            UIView.animateWithDuration(self.animationDuration!, animations: _anim_doing, completion: _anim_complete)
        }
    }
    
    // =================================
    // view lifecycle
    // =================================
    
    
    override func viewDidLoad() {
        super.viewDidLoad()
        self.view.autoresizingMask = UIViewAutoresizing.FlexibleWidth | UIViewAutoresizing.FlexibleHeight
        var imageView = UIImageView(frame: self.view.bounds)
        imageView.image = self.backgroundImage
        imageView.contentMode = UIViewContentMode.ScaleAspectFill
        imageView.autoresizingMask = UIViewAutoresizing.FlexibleWidth | UIViewAutoresizing.FlexibleHeight
        self.backgroundImageView = imageView
        var button = UIButton(frame: CGRectNull)
        button.addTarget(self, action:"hideMenuViewController", forControlEvents:UIControlEvents.TouchUpInside)
        self.contentButton = button
        
        self.view.addSubview(self.backgroundImageView!)
        self.view.addSubview(self.menuViewContainer!)
        self.view.addSubview(self.contentViewContainer!)
        
        self.menuViewContainer!.frame = self.view.bounds
        self.menuViewContainer!.autoresizingMask = UIViewAutoresizing.FlexibleWidth | UIViewAutoresizing.FlexibleHeight
        if (self.leftMenuViewController != nil) {
            self.addChildViewController(self.leftMenuViewController!)
            self.leftMenuViewController!.view.frame = self.view.bounds
            self.leftMenuViewController!.view.autoresizingMask = UIViewAutoresizing.FlexibleWidth | UIViewAutoresizing.FlexibleHeight
            self.menuViewContainer!.addSubview(self.leftMenuViewController!.view)
            self.leftMenuViewController!.didMoveToParentViewController(self)
        }
        
        if (self.rightMenuViewController != nil) {
            self.addChildViewController(self.rightMenuViewController!)
            self.rightMenuViewController!.view.frame = self.view.bounds
            self.rightMenuViewController!.view.autoresizingMask = UIViewAutoresizing.FlexibleWidth | UIViewAutoresizing.FlexibleHeight
            self.menuViewContainer!.addSubview(self.rightMenuViewController!.view)
            self.rightMenuViewController!.didMoveToParentViewController(self)
        }
        
        self.contentViewContainer!.frame = self.view.bounds
        self.contentViewContainer!.autoresizingMask = UIViewAutoresizing.FlexibleWidth | UIViewAutoresizing.FlexibleHeight
        
        self.addChildViewController(self.contentViewController!)
        self.contentViewController!.view.frame = self.view.bounds
        self.contentViewContainer!.addSubview(self.contentViewController!.view)
        self.contentViewController!.didMoveToParentViewController(self)
        
        self.menuViewContainer!.alpha = !self.fadeMenuView! ? 1 : 0
        if (self.scaleBackgroundImageView != nil) {
            self.backgroundImageView!.transform = CGAffineTransformMakeScale(1.7, 1.7)
        }
        self.__addMenuViewControllerMotionEffects()
        
        if (self.panGestureEnabled!) {
            self.view.multipleTouchEnabled = false
            var panGestureRecognizer = UIPanGestureRecognizer(target: self, action: "__panGestureRecognized:")
            panGestureRecognizer.delegate = self
            self.view.addGestureRecognizer(panGestureRecognizer)
        }
        
        self.__updateContentViewShadow()
    }
    
    // =================================
    // privte method
    // =================================
    
    func __presentMenuViewContainerWithMenuViewController(menuViewController: UIViewController?) {
        self.menuViewContainer!.transform = CGAffineTransformIdentity
        if (self.scaleBackgroundImageView!) {
            self.backgroundImageView!.transform = CGAffineTransformIdentity
            self.backgroundImageView!.frame = self.view.bounds
        }
        self.menuViewContainer!.frame = self.view.bounds
        if (self.scaleMenuView!) {
            self.menuViewContainer!.transform = self.menuViewControllerTransformation!
        }
        self.menuViewContainer!.alpha = !self.fadeMenuView! ? 1 : 0
        if (self.scaleBackgroundImageView!) {
            self.backgroundImageView!.transform = CGAffineTransformMakeScale(1.7, 1.7)
        }
        self.delegate?.sideMenu?(self, willShowMenuViewController: menuViewController)
    }
    
    
    func __showLeftMenuViewController() {
        if (self.leftMenuViewController == nil) {
            return
        }
        self.leftMenuViewController!.view.hidden = false
        if (self.rightMenuViewController != nil) {
            self.rightMenuViewController!.view.hidden = true
        }
        self.view.window!.endEditing(true)
        self.__addContentButton()
        self.__updateContentViewShadow()
        self.__resetContentViewScale()
        
        func _anim_doing() {
            if (self.scaleContentView!) {
                self.contentViewContainer!.transform = CGAffineTransformMakeScale(CGFloat(self.contentViewScaleValue!), CGFloat(self.contentViewScaleValue!))
            } else {
                self.contentViewContainer!.transform = CGAffineTransformIdentity;
            }
            self.contentViewContainer!.center = CGPointMake((UIInterfaceOrientationIsLandscape(UIApplication.sharedApplication().statusBarOrientation) ? self.contentViewInLandscapeOffsetCenterX! + CGRectGetHeight(self.view.frame) : self.contentViewInPortraitOffsetCenterX! + CGRectGetWidth(self.view.frame)), self.contentViewContainer!.center.y);
            
            self.menuViewContainer!.alpha = !self.fadeMenuView! ? 0 : 1.0
            self.menuViewContainer!.transform = CGAffineTransformIdentity
            if (self.scaleBackgroundImageView!) {
                self.backgroundImageView!.transform = CGAffineTransformIdentity
            }
        }
        
        func _anim_complete(var finish: Bool) {
            self.__addContentViewControllerMotionEffects()
            if (!self.visible!) {
                self.delegate?.sideMenu?(self, didShowMenuViewController: self.leftMenuViewController!)
            }
            self.visible = true
            self.leftMenuVisible = true
        }
    
        UIView.animateWithDuration(self.animationDuration!, animations: _anim_doing, completion: _anim_complete)
    
        self.__statusBarNeedsAppearanceUpdate()
    }
    
    func __showRightMenuViewController() {
        if (self.rightMenuViewController == nil) {
            return
        }
        if (self.leftMenuViewController != nil) {
            self.leftMenuViewController!.view.hidden = true
        }
        self.rightMenuViewController!.view.hidden = false
        self.view.window!.endEditing(true)
        self.__addContentButton()
        self.__updateContentViewShadow()
        self.__resetContentViewScale()
        UIApplication.sharedApplication().beginIgnoringInteractionEvents()
        func _anim_doing() {
            if (self.scaleContentView!) {
                self.contentViewContainer!.transform = CGAffineTransformMakeScale(self.contentViewScaleValue!, self.contentViewScaleValue!)
            } else {
                self.contentViewContainer!.transform = CGAffineTransformIdentity
            }
            self.contentViewContainer!.center = CGPointMake((UIInterfaceOrientationIsLandscape(UIApplication.sharedApplication().statusBarOrientation) ? -self.contentViewInLandscapeOffsetCenterX! : -self.contentViewInPortraitOffsetCenterX!), self.contentViewContainer!.center.y)
            
            self.menuViewContainer!.alpha = !self.fadeMenuView! ? 0 : 1.0
            self.menuViewContainer!.transform = CGAffineTransformIdentity
            if (self.scaleBackgroundImageView!) {
                self.backgroundImageView!.transform = CGAffineTransformIdentity
            }
        }
        
        func _anim_complete(var finish: Bool) {
            if (!self.rightMenuVisible!) {
                self.delegate?.sideMenu?(self, didShowMenuViewController: self.rightMenuViewController!)
            }
            self.visible = !(self.contentViewContainer!.frame.size.width == self.view.bounds.size.width && self.contentViewContainer!.frame.size.height == self.view.bounds.size.height && self.contentViewContainer!.frame.origin.x == 0 && self.contentViewContainer!.frame.origin.y == 0)
            self.rightMenuVisible = self.visible
            UIApplication.sharedApplication().endIgnoringInteractionEvents()
            self.__addContentViewControllerMotionEffects()
        }
        
        UIView.animateWithDuration(self.animationDuration!, animations: _anim_doing, completion: _anim_complete)
        self.__statusBarNeedsAppearanceUpdate()
    }
    
    func __hideViewController(viewController: UIViewController?) {
        viewController!.willMoveToParentViewController(nil)
        viewController!.view.removeFromSuperview()
        viewController!.removeFromParentViewController()
    }
    
    func __hideMenuViewControllerAnimated(animated: Bool) {
        var rightMenuVisible = self.rightMenuVisible
        self.delegate?.sideMenu?(self, willHideMenuViewController: rightMenuVisible! ? self.rightMenuViewController : self.leftMenuViewController)
        self.visible = false
        self.leftMenuVisible = false
        self.rightMenuVisible = false
        self.contentButton!.removeFromSuperview()
        weak var weakSelf = self;

        var animationBlock: () -> Void = {
            var strongSelf = weakSelf
            if (strongSelf == nil) {
                return
            }
            strongSelf!.contentViewContainer!.transform = CGAffineTransformIdentity
            strongSelf!.contentViewContainer!.frame = strongSelf!.view.bounds
            if (strongSelf!.scaleMenuView!) {
                strongSelf!.menuViewContainer!.transform = strongSelf!.menuViewControllerTransformation!
            }
            strongSelf!.menuViewContainer!.alpha = !self.fadeMenuView! ? 1 : 0
            if (strongSelf!.scaleBackgroundImageView!) {
                strongSelf!.backgroundImageView!.transform = CGAffineTransformMakeScale(1.7, 1.7)
            }
            if (strongSelf!.parallaxEnabled!) {
                for effect in strongSelf!.contentViewContainer!.motionEffects! {
                    strongSelf!.contentViewContainer!.removeMotionEffect(effect as UIMotionEffect)
                }
            }
        }
    
        var completionBlock: () -> Void = {
            var strongSelf = weakSelf;
            if (strongSelf == nil) {
                return
            }
            if (!strongSelf!.visible!) {
                strongSelf!.delegate?.sideMenu?(self, didHideMenuViewController: rightMenuVisible! ? strongSelf!.rightMenuViewController : strongSelf!.leftMenuViewController)
            }
        }
    
        if (animated) {
            UIApplication.sharedApplication().beginIgnoringInteractionEvents()
            func _anim_doing() {
                animationBlock()
            }
            
            func _anim_complete (var finish: Bool) {
                UIApplication.sharedApplication().endIgnoringInteractionEvents()
                completionBlock()
            }
            UIView.animateWithDuration(self.animationDuration!, animations: _anim_doing, completion: _anim_complete)
            
        } else {
            animationBlock()
            completionBlock()
        }
        self.__statusBarNeedsAppearanceUpdate()
    }
    
    func __addContentButton() {
        if (self.contentButton!.superview != nil) {
            return
        }
    
        self.contentButton!.autoresizingMask = UIViewAutoresizing.None
        self.contentButton!.frame = self.contentViewContainer!.bounds
        self.contentButton!.autoresizingMask = UIViewAutoresizing.FlexibleWidth | UIViewAutoresizing.FlexibleHeight
        self.contentViewContainer!.addSubview(self.contentButton!)
    }
    
    func __statusBarNeedsAppearanceUpdate() {
        func _anim_doing() {
            setNeedsStatusBarAppearanceUpdate()
        }
        UIView.animateWithDuration(0.3, animations: _anim_doing)
    }
    
    func __updateContentViewShadow() {
        if (self.contentViewShadowEnabled!) {
            var layer = self.contentViewContainer!.layer
            var path = UIBezierPath(rect: layer.bounds)
            layer.shadowPath = path.CGPath
            layer.shadowColor = self.contentViewShadowColor!.CGColor
            layer.shadowOffset = self.contentViewShadowOffset!
            layer.shadowOpacity = Float(self.contentViewShadowOpacity!)
            layer.shadowRadius = self.contentViewShadowRadius!
        }
    }
    
    func __resetContentViewScale() {
        var t = self.contentViewContainer!.transform
        var scale = sqrt(t.a * t.a + t.c * t.c)
        var frame = self.contentViewContainer!.frame
        self.contentViewContainer!.transform = CGAffineTransformIdentity
        self.contentViewContainer!.transform = CGAffineTransformMakeScale(scale, scale)
        self.contentViewContainer!.frame = frame
    }

    // =================================
    // ios7 motion
    // =================================

    func __addMenuViewControllerMotionEffects() {
        if (self.parallaxEnabled!) {
            if (self.menuViewContainer!.motionEffects != nil) {
                for effect in self.menuViewContainer!.motionEffects! {
                    self.menuViewContainer!.removeMotionEffect(effect as UIMotionEffect)
                }
            }
            var interpolationHorizontal = UIInterpolatingMotionEffect(keyPath: "center.x", type: UIInterpolatingMotionEffectType.TiltAlongHorizontalAxis)
            interpolationHorizontal.minimumRelativeValue = self.parallaxMenuMinimumRelativeValue
            interpolationHorizontal.maximumRelativeValue = self.parallaxMenuMaximumRelativeValue
    
            var interpolationVertical = UIInterpolatingMotionEffect(keyPath: "center.y", type: UIInterpolatingMotionEffectType.TiltAlongVerticalAxis)
            interpolationVertical.minimumRelativeValue = self.parallaxMenuMinimumRelativeValue
            interpolationVertical.maximumRelativeValue = self.parallaxMenuMaximumRelativeValue
    
            self.menuViewContainer!.addMotionEffect(interpolationHorizontal)
            self.menuViewContainer!.addMotionEffect(interpolationVertical)
        }
    }
    
    func __addContentViewControllerMotionEffects() {
        if (self.parallaxEnabled!) {
            for effect in self.contentViewContainer!.motionEffects! {
                self.contentViewContainer!.removeMotionEffect(effect as UIMotionEffect)
            }
            
            func _anim_doing() {
                var interpolationHorizontal = UIInterpolatingMotionEffect(keyPath: "center.x", type: UIInterpolatingMotionEffectType.TiltAlongHorizontalAxis)
                interpolationHorizontal.minimumRelativeValue = self.parallaxContentMinimumRelativeValue
                interpolationHorizontal.maximumRelativeValue = self.parallaxContentMaximumRelativeValue
                var interpolationVertical = UIInterpolatingMotionEffect(keyPath: "center.y", type: UIInterpolatingMotionEffectType.TiltAlongVerticalAxis)
                interpolationVertical.minimumRelativeValue = self.parallaxContentMinimumRelativeValue
                interpolationVertical.maximumRelativeValue = self.parallaxContentMaximumRelativeValue
                self.contentViewContainer!.addMotionEffect(interpolationHorizontal)
                self.contentViewContainer!.addMotionEffect(interpolationVertical)
            }
            UIView.animateWithDuration(0.2, animations: _anim_doing)
        }
    }
    
    // =================================
    // gesture delegate
    // =================================
    
    func gestureRecognizer(gestureRecognizer: UIGestureRecognizer, shouldReceiveTouch touch: UITouch) -> Bool {
        if (self.interactivePopGestureRecognizerEnabled! && (self.contentViewController is UINavigationController)) {
            var navigationController = self.contentViewController as UINavigationController
            if (navigationController.viewControllers.count > 1 && navigationController.interactivePopGestureRecognizer.enabled) {
                return false;
            }
        }
        
        if (self.panFromEdge! && (gestureRecognizer is UIPanGestureRecognizer) && !self.visible!) {
            var point = touch.locationInView(gestureRecognizer.view)
            if (point.x < self.panMinimumEdgeSize! || point.x > self.view.frame.size.width - self.panMinimumEdgeSize!) {
                return true
            } else {
                return false
            }
        }
        
        return true
    }
    

    // =================================
    // Pan gesture recognizer
    // =================================
    
    func __panGestureRecognized(recognizer: UIPanGestureRecognizer) {
        self.delegate?.sideMenu?(self, didRecognizePanGesture: recognizer)
        if (!self.panGestureEnabled!) {
            return
        }
        var point = recognizer.translationInView(self.view)
        if (recognizer.state == UIGestureRecognizerState.Began) {
            self.__updateContentViewShadow()
            self.originalPoint = CGPointMake(self.contentViewContainer!.center.x - CGRectGetWidth(self.contentViewContainer!.bounds) / 2.0,
                self.contentViewContainer!.center.y - CGRectGetHeight(self.contentViewContainer!.bounds) / 2.0)
            self.menuViewContainer!.transform = CGAffineTransformIdentity
            if (self.scaleBackgroundImageView!) {
                self.backgroundImageView!.transform = CGAffineTransformIdentity
                self.backgroundImageView!.frame = self.view.bounds
            }
            self.menuViewContainer!.frame = self.view.bounds
            self.__addContentButton()
            self.view.window!.endEditing(true)
            self.didNotifyDelegate = false
        }
    
        if (recognizer.state == UIGestureRecognizerState.Changed) {
            var delta: CGFloat = 0.0
            if (self.visible!) {
                delta = self.originalPoint!.x != 0 ? (point.x + self.originalPoint!.x) / self.originalPoint!.x : 0
            } else {
                delta = point.x / self.view.frame.size.width
            }
            delta = min(fabs(delta), 1.6)
    
            var contentViewScale:CGFloat = self.scaleContentView! ? 1 - ((1 - self.contentViewScaleValue!) * delta) : 1
    
            var backgroundViewScale: CGFloat = 1.7 - (0.7 * delta)
            var menuViewScale: CGFloat = 1.5 - (0.5 * delta)
    
            if (!self.bouncesHorizontally!) {
                contentViewScale = max(contentViewScale, self.contentViewScaleValue!)
                backgroundViewScale = max(backgroundViewScale, 1.0)
                menuViewScale = max(menuViewScale, 1.0)
            }
    
            self.menuViewContainer!.alpha = !self.fadeMenuView! ? 0 : delta;
    
            if (self.scaleBackgroundImageView!) {
                self.backgroundImageView!.transform = CGAffineTransformMakeScale(backgroundViewScale, backgroundViewScale)
            }
    
            if (self.scaleMenuView!) {
                self.menuViewContainer!.transform = CGAffineTransformMakeScale(menuViewScale, menuViewScale)
            }
    
            if (self.scaleBackgroundImageView!) {
                if (backgroundViewScale < 1) {
                    self.backgroundImageView!.transform = CGAffineTransformIdentity
                }
            }
    
            if (!self.bouncesHorizontally! && self.visible!) {
                if (self.contentViewContainer!.frame.origin.x > self.contentViewContainer!.frame.size.width / 2.0) {
                    point.x = min(0.0, point.x)
                }
                if (self.contentViewContainer!.frame.origin.x < -(self.contentViewContainer!.frame.size.width / 2.0)) {
                    point.x = max(0.0, point.x)
                }
            }

            if (point.x < 0) {
                point.x = max(point.x, -UIScreen.mainScreen().bounds.size.height)
            } else {
                point.x = min(point.x, UIScreen.mainScreen().bounds.size.height)
            }
            recognizer.setTranslation(point, inView:self.view)
    
            if (!self.didNotifyDelegate!) {
                if (point.x > 0) {
                    if (!self.visible!) {
                        self.delegate?.sideMenu?(self, willShowMenuViewController: leftMenuViewController)
                    }
                }
                if (point.x < 0) {
                    if (!self.visible!) {
                        self.delegate?.sideMenu?(self, willShowMenuViewController: rightMenuViewController)
                    }
                }
                self.didNotifyDelegate = true
            }
    
            if (contentViewScale > 1) {
                var oppositeScale: CGFloat = (1 - (contentViewScale - 1))
                self.contentViewContainer!.transform = CGAffineTransformMakeScale(oppositeScale, oppositeScale)
                self.contentViewContainer!.transform = CGAffineTransformTranslate(self.contentViewContainer!.transform, point.x, 0)
            } else {
                self.contentViewContainer!.transform = CGAffineTransformMakeScale(contentViewScale, contentViewScale)
                self.contentViewContainer!.transform = CGAffineTransformTranslate(self.contentViewContainer!.transform, point.x, 0)
            }
            if (self.leftMenuViewController != nil) {
                self.leftMenuViewController!.view.hidden = self.contentViewContainer!.frame.origin.x < 0
            }
            if (self.rightMenuViewController != nil) {
                self.rightMenuViewController!.view.hidden = self.contentViewContainer!.frame.origin.x > 0
            }
    
            if (self.leftMenuViewController == nil && self.contentViewContainer!.frame.origin.x > 0) {
                self.contentViewContainer!.transform = CGAffineTransformIdentity
                self.contentViewContainer!.frame = self.view.bounds
                self.visible = false
                self.leftMenuVisible = false
            } else if (self.rightMenuViewController == nil && self.contentViewContainer!.frame.origin.x < 0) {
                self.contentViewContainer!.transform = CGAffineTransformIdentity
                self.contentViewContainer!.frame = self.view.bounds
                self.visible = false
                self.rightMenuVisible = false
            }
    
            self.__statusBarNeedsAppearanceUpdate()
        }
    
        if (recognizer.state == UIGestureRecognizerState.Ended) {
            self.didNotifyDelegate = false
            
            if (self.panMinimumOpenThreshold! > 0 && ((self.contentViewContainer!.frame.origin.x < 0 && self.contentViewContainer!.frame.origin.x > CGFloat(-self.panMinimumOpenThreshold!)) || (self.contentViewContainer!.frame.origin.x > 0 && self.contentViewContainer!.frame.origin.x < CGFloat(self.panMinimumOpenThreshold!)))) {
                self.hideMenuViewController()
            } else if (self.contentViewContainer!.frame.origin.x == 0) {
                self.__hideMenuViewControllerAnimated(false)
            } else {
                if (recognizer.velocityInView(self.view).x > 0) {
                    if (self.contentViewContainer!.frame.origin.x < 0) {
                        self.hideMenuViewController()
                    } else {
                        if (self.leftMenuViewController != nil) {
                            self.__showLeftMenuViewController()
                        }
                    }
                } else {
                    if (self.contentViewContainer!.frame.origin.x < 20) {
                        if (self.rightMenuViewController != nil) {
                            self.__showRightMenuViewController()
                        }
                    } else {
                        self.hideMenuViewController()
                    }
                }
            }
        }
    }
    
    // =================================
    // rotation
    // =================================
    override func shouldAutorotate() -> Bool {
        return self.contentViewController!.shouldAutorotate()
    }
    
    override func willAnimateRotationToInterfaceOrientation(toInterfaceOrientation: UIInterfaceOrientation, duration: NSTimeInterval) {
        if (self.visible!) {
            self.menuViewContainer!.bounds = self.view.bounds
            self.contentViewContainer!.transform = CGAffineTransformIdentity
            self.contentViewContainer!.frame = self.view.bounds
            
            if (self.scaleContentView!) {
                self.contentViewContainer!.transform = CGAffineTransformMakeScale(self.contentViewScaleValue!, self.contentViewScaleValue!)
            } else {
                self.contentViewContainer!.transform = CGAffineTransformIdentity
            }
            
            var center: CGPoint?
            if (self.leftMenuVisible!) {
                center = CGPointMake((UIDeviceOrientationIsLandscape(UIDevice.currentDevice().orientation) ? self.contentViewInLandscapeOffsetCenterX! + CGRectGetHeight(self.view.frame) : self.contentViewInPortraitOffsetCenterX! + CGRectGetWidth(self.view.frame)), self.contentViewContainer!.center.y)
            } else {
                center = CGPointMake((UIDeviceOrientationIsLandscape(UIDevice.currentDevice().orientation) ? -self.contentViewInLandscapeOffsetCenterX! : -self.contentViewInPortraitOffsetCenterX!), self.contentViewContainer!.center.y)
            }
            
            self.contentViewContainer!.center = center!
        }
        
        self.__updateContentViewShadow()
    }
    
    // =================================
    // status bar
    // =================================
    
    override func preferredStatusBarStyle() -> UIStatusBarStyle {
        var statusBarStyle = UIStatusBarStyle.Default
        statusBarStyle = self.visible! ? self.menuPreferredStatusBarStyle! : self.contentViewController!.preferredStatusBarStyle()
        if (self.contentViewContainer!.frame.origin.y > 10) {
            statusBarStyle = self.menuPreferredStatusBarStyle!
        } else {
            statusBarStyle = self.contentViewController!.preferredStatusBarStyle()
        }
        return statusBarStyle
    }
    
    override func prefersStatusBarHidden() -> Bool {
        var statusBarHidden = false
        statusBarHidden = self.visible! ? self.menuPrefersStatusBarHidden! : self.contentViewController!.prefersStatusBarHidden()
        if (self.contentViewContainer!.frame.origin.y > 10) {
            statusBarHidden = self.menuPrefersStatusBarHidden!
        } else {
            statusBarHidden = self.contentViewController!.prefersStatusBarHidden()
        }
        return statusBarHidden
    }
    
    override func preferredStatusBarUpdateAnimation() -> UIStatusBarAnimation {
        var statusBarAnimation = UIStatusBarAnimation.None
        statusBarAnimation = self.visible! ? self.leftMenuViewController!.preferredStatusBarUpdateAnimation() : self.contentViewController!.preferredStatusBarUpdateAnimation()
        if (self.contentViewContainer!.frame.origin.y > 10) {
            statusBarAnimation = self.leftMenuViewController!.preferredStatusBarUpdateAnimation()
        } else {
            statusBarAnimation = self.contentViewController!.preferredStatusBarUpdateAnimation()
        }

        return statusBarAnimation

    }
    

}

