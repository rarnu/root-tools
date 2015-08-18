package com.rarnu.devlib.common;

import android.view.View;
import android.view.ViewGroup.LayoutParams;
import com.rarnu.devlib.component.SlidingMenu;

public interface ISliding {

    void setBehindContentView(View view, LayoutParams layoutParams);

    void setBehindContentView(View view);

    void setBehindContentView(int layoutResID);

    SlidingMenu getSlidingMenu();

    void toggle();

    void showContent();

    void showMenu();

    void showSecondaryMenu();

    void setSlidingActionBarEnabled(boolean slidingActionBarEnabled);

}
