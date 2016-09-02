package com.rarnu.tools.neo.base;

import android.os.Bundle;
import android.view.Menu;

public interface IIntf {

    int getBarTitle();
    String getCustomTitle();
    void initComponents();
    void initEvents();
    void initLogic();
    int getFragmentLayoutResId();
    String getMainActivityName();
    void initMenu(Menu menu);
    void onGetNewArguments(Bundle bn);
    Bundle getFragmentState();

}
