package com.rarnu.ucloud.android.fragment;

import android.app.Fragment;
import android.content.Intent;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.net.Uri;
import android.os.Bundle;
import android.view.Menu;
import android.view.MenuItem;
import android.widget.ShareActionProvider;
import com.rarnu.devlib.base.BaseTabFragment;
import com.rarnu.ucloud.android.R;
import com.rarnu.ucloud.android.common.MenuIds;
import com.rarnu.utils.ImageUtils;

import java.io.File;
import java.util.List;

public class MainFragment extends BaseTabFragment {

    MenuItem miShare;
    ShareActionProvider shareAction;

    @Override
    public void initFragmentList(List<Fragment> listFragment) {
        listFragment.add(new ServerFragment());
        listFragment.add(new FlowFragment());
        listFragment.add(new CostFragment());
        listFragment.add(new ServiceFragment());
        listFragment.add(new SettingsFragment());
    }

    @Override
    public int getBarTitle() {
        return R.string.app_name;
    }

    @Override
    public int getBarTitleWithPath() {
        return R.string.app_name;
    }

    @Override
    public String getCustomTitle() {
        return null;
    }

    @Override
    public String getMainActivityName() {
        return "";
    }

    @Override
    public void initMenu(Menu menu) {
        miShare = menu.add(0, MenuIds.MENUID_SHARE, 99, R.string.menu_share);
        miShare.setIcon(android.R.drawable.ic_menu_share);
        miShare.setShowAsAction(MenuItem.SHOW_AS_ACTION_IF_ROOM);
        miShare.setActionProvider(shareAction);
    }

    @Override
    public void onGetNewArguments(Bundle bn) {
    }

    @Override
    public Bundle getFragmentState() {
        return null;
    }

    @Override
    public void initComponents() {
        super.initComponents();
        shareAction = new ShareActionProvider(getActivity());
        shareAction.setShareIntent(getShareIntent());
        shareAction.setShareHistoryFileName(ShareActionProvider.DEFAULT_SHARE_HISTORY_FILE_NAME);
    }

    private Intent getShareIntent() {
        Intent shareIntent = new Intent(Intent.ACTION_SEND);
        shareIntent.setType("image/*");
        shareIntent.putExtra(Intent.EXTRA_TEXT, getString(R.string.menu_share));
        return shareIntent;
    }
}
