import UIKit

extension UIViewController {
    
    var sideMenuViewController: SideMenu? {
        get {
            var iter: UIViewController? = self.parentViewController
            while (iter != nil) {
                if (iter is SideMenu) {
                    return iter as? SideMenu
                } else if (iter!.parentViewController != nil && iter!.parentViewController != iter) {
                    iter = iter!.parentViewController
                } else {
                    iter = nil;
                }
            }
            return nil;
        }
    }
    
    @IBAction func presentLeftMenuViewController(sender: AnyObject) {
        self.sideMenuViewController!.presentLeftMenuViewController()
    }
    
    @IBAction func presentRightMenuViewController(sender: AnyObject) {
        self.sideMenuViewController!.presentRightMenuViewController()
    }
}
