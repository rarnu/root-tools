package com.rarnu.devlib.intf;

import com.rarnu.devlib.component.SwipeLayout;

import java.util.List;

public interface SwipeItemManageIntf {

    public void openItem(int position);

    public void closeItem(int position);

    public void closeAllExcept(SwipeLayout layout);
    
    public void closeAllItems();

    public List<Integer> getOpenItems();

    public List<SwipeLayout> getOpenLayouts();

    public void removeShownLayouts(SwipeLayout layout);

    public boolean isOpen(int position);

    public SwipeLayout.Mode getMode();

    public void setMode(SwipeLayout.Mode mode);
}
