//
//  UIUtils.swift
//  YibanJob
//
//  Created by rarnu on 1/10/15.
//  Copyright (c) 2015 yiban. All rights reserved.
//

import UIKit

class UIUtils: NSObject {
    
    class func getDefaultColor() -> UIColor {
        return UIColor(red: 0x00, green: 0xA5 / 0xFF, blue: 0xED / 0xFF, alpha: 1)
    }
    
    class func getStatusBarRect() -> CGRect {
        return UIApplication.sharedApplication().statusBarFrame
    }
    
    class func getScreenRect() -> CGRect {
        return UIScreen.mainScreen().bounds
    }
    
    class func setNavBar(nav: UINavigationBar) {
        nav.barTintColor = getDefaultColor()
        nav.tintColor = UIColor.whiteColor()
        nav.titleTextAttributes = NSDictionary(objectsAndKeys: UIColor.whiteColor(), NSForegroundColorAttributeName)
    }
    
    class func setTabBar(tab: UITabBar) {
        tab.tintColor = UIUtils.getDefaultColor()
        tab.barTintColor = UIColor.whiteColor()
    }

    class func setTextTitle(controller: UIViewController, txt: UILabel) -> (CGFloat, CGFloat) {
        var y = UIUtils.getStatusBarRect().height
        var w = UIUtils.getScreenRect().width
        var h: CGFloat = 44.0
        var t: CGFloat = y + h
        var rh: CGFloat = UIUtils.getScreenRect().height - t
        var backColor = getDefaultColor()
        txt.frame = CGRectMake(0, y, w, h)
        txt.backgroundColor = backColor
        txt.textColor = UIColor.whiteColor()
        h = UIUtils.getStatusBarRect().height
        var txtImmersion = UILabel(frame: CGRectMake(0, 0, w, h))
        txtImmersion.backgroundColor = backColor
        controller.view.addSubview(txtImmersion)
        return (t, rh)
    }
}
