package com.yugioh.android.fragments;

import android.content.Intent;
import android.content.Loader;
import android.content.Loader.OnLoadCompleteListener;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.view.Menu;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.*;
import android.widget.AdapterView.OnItemClickListener;
import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.utils.ResourceUtils;
import com.yugioh.android.*;
import com.yugioh.android.adapter.RecommandAdapter;
import com.yugioh.android.adapter.RightMenuAdapter;
import com.yugioh.android.classes.RecommandInfo;
import com.yugioh.android.classes.RightMenuItem;
import com.yugioh.android.classes.UpdateInfo;
import com.yugioh.android.loader.RecommandLoader;
import com.yugioh.android.utils.RecommandUtils;
import com.yugioh.android.utils.UpdateUtils;

import java.util.ArrayList;
import java.util.List;

public class RightMenuFragment extends BaseFragment implements OnItemClickListener, OnLoadCompleteListener<List<RecommandInfo>>, OnClickListener {

    final Handler hUpdate = new Handler() {
        @Override
        public void handleMessage(Message msg) {
            if (msg.what == 1) {
                updateInfo = (UpdateInfo) msg.obj;
                updateMenu(updateInfo);

            }
            super.handleMessage(msg);
        }
    };
    ListView lvAbout, lvSettings;
    List<String> listAbout;
    ArrayAdapter<String> adapterAbout;
    List<RightMenuItem> listSettings;
    RightMenuAdapter adapterSettings;
    UpdateInfo updateInfo;
    ListView lvRecommand;
    List<RecommandInfo> listRecommand;
    RecommandLoader loaderRecommand;
    RecommandAdapter adapterRecommand;
    ImageView imgSettings;

    public RightMenuFragment() {
        super();
        tagText = ResourceUtils.getString(R.string.tag_menu_right);
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
    public void initComponents() {
        imgSettings = (ImageView) innerView.findViewById(R.id.imgSettings);
        lvRecommand = (ListView) innerView.findViewById(R.id.lvRecommand);
        lvSettings = (ListView) innerView.findViewById(R.id.lvSettings);
        lvAbout = (ListView) innerView.findViewById(R.id.lvAbout);
        listAbout = new ArrayList<String>();
        listAbout.add(getString(R.string.rm_feedback));
        listAbout.add(getString(R.string.rm_about));
        adapterAbout = new ArrayAdapter<String>(getActivity(), R.layout.item_menu, listAbout);
        lvAbout.setAdapter(adapterAbout);

        listSettings = new ArrayList<RightMenuItem>();
        RightMenuItem itemUpdate = new RightMenuItem();
        itemUpdate.type = 0;
        itemUpdate.name = getString(R.string.rm_update);
        itemUpdate.value = 0;
        listSettings.add(itemUpdate);
        adapterSettings = new RightMenuAdapter(getActivity(), listSettings);
        lvSettings.setAdapter(adapterSettings);

        loaderRecommand = new RecommandLoader(getActivity());
        listRecommand = new ArrayList<RecommandInfo>();
        adapterRecommand = new RecommandAdapter(getActivity(), listRecommand);
        lvRecommand.setAdapter(adapterRecommand);
    }

    @Override
    public void initEvents() {
        lvAbout.setOnItemClickListener(this);
        lvSettings.setOnItemClickListener(this);
        lvRecommand.setOnItemClickListener(this);
        loaderRecommand.registerListener(0, this);
        imgSettings.setOnClickListener(this);
    }

    @Override
    public void initLogic() {
        loaderRecommand.startLoading();
    }

    @Override
    public void onResume() {
        super.onResume();
        UpdateUtils.checkUpdateT(getActivity(), hUpdate);
    }

    @Override
    public int getFragmentLayoutResId() {
        return R.layout.menu_right;
    }

    @Override
    public String getMainActivityName() {
        return MainActivity.class.getName();
    }

    @Override
    public void initMenu(Menu menu) {

    }

    @Override
    public void onGetNewArguments(Bundle bn) {

    }

    @Override
    public void onItemClick(AdapterView<?> parent, View view, int position,
                            long id) {
        switch (parent.getId()) {
            case R.id.lvSettings:
                switch (position) {
                    case 0:
                        if (updateInfo != null && ((updateInfo.getUpdateApk() + updateInfo.getUpdateData()) != 0)) {
                            Intent inUpdate = new Intent(getActivity(), UpdateActivity.class);
                            inUpdate.putExtra("update", updateInfo);
                            startActivity(inUpdate);
                        } else {
                            Toast.makeText(getActivity(), R.string.update_no, Toast.LENGTH_SHORT).show();
                        }
                        break;
                }
                break;
            case R.id.lvAbout:
                switch (position) {
                    case 0:
                        startActivity(new Intent(getActivity(), FeedbackActivity.class));
                        break;
                    case 1:
                        startActivity(new Intent(getActivity(), AboutActivity.class));
                        break;
                }
                break;
            case R.id.lvRecommand:
                doRecommand(listRecommand.get(position));
                break;
        }
    }

    private void doRecommand(RecommandInfo info) {
        switch (info.jumpMode) {
            case 0:
                RecommandUtils.showUrlRecommand(getActivity(), info);
                break;
            case 1:
                RecommandUtils.showTextRecommand(getActivity(), info);
                break;
        }
    }

    @Override
    public String getCustomTitle() {
        return null;
    }

    private void updateMenu(UpdateInfo updateInfo) {
        this.updateInfo = updateInfo;
        if (updateInfo != null) {
            listSettings.get(0).value = updateInfo.getUpdateApk() + updateInfo.getUpdateData();
        }
        adapterSettings.setNewList(listSettings);
    }

    @Override
    public void onLoadComplete(Loader<List<RecommandInfo>> loader, List<RecommandInfo> data) {
        listRecommand.clear();
        if (data != null) {
            listRecommand.addAll(data);
        }
        if (getActivity() != null) {
            adapterRecommand.setNewList(listRecommand);
        }
    }

    @Override
    public void onClick(View v) {
        switch (v.getId()) {
            case R.id.imgSettings:
                startActivity(new Intent(getActivity(), SettingsActivity.class));
                break;
        }

    }

    @Override
    public Bundle getFragmentState() {
        return null;
    }

}
