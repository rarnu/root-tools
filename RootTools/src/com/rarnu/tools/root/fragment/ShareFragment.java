package com.rarnu.tools.root.fragment;

import android.content.Intent;
import android.content.pm.ApplicationInfo;
import android.os.Bundle;
import android.util.Log;
import android.view.Menu;
import android.view.View;
import android.widget.AdapterView;
import android.widget.GridView;
import android.widget.ImageView;
import com.rarnu.devlib.base.BaseDialogFragment;
import com.rarnu.tools.root.GlobalInstance;
import com.rarnu.tools.root.MainActivity;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.adapter.ShareAdapter;
import com.rarnu.tools.root.common.ShareItem;
import com.rarnu.utils.UIUtils;

import java.util.ArrayList;
import java.util.List;

public class ShareFragment extends BaseDialogFragment implements View.OnClickListener, AdapterView.OnItemClickListener {

    ImageView btnCancel;
    GridView gvShare;
    List<ShareItem> list;
    ShareAdapter adapter;

    @Override
    public int getBarTitle() {
        return 0;
    }

    @Override
    public int getBarTitleWithPath() {
        return 0;
    }

    @Override
    public String getCustomTitle() {
        return null;
    }

    @Override
    public void initComponents() {
        btnCancel = (ImageView) innerView.findViewById(R.id.btnCancel);
        gvShare = (GridView) innerView.findViewById(R.id.gvShare);
        list = new ArrayList<ShareItem>();
        String[] sharePackage = getResources().getStringArray(R.array.array_share_list);
        String[] shareClass = getResources().getStringArray(R.array.array_share_class);
        ApplicationInfo appinfo = null;
        String[] packageNames = null;
        for (int i = 0; i < sharePackage.length; i++) {
            if (sharePackage[i].contains("|")) {
                packageNames = sharePackage[i].split("\\|");
            } else {
                packageNames = new String[]{sharePackage[i]};
            }
            for (int j = 0; j < packageNames.length; j++) {
                Log.e("test package", packageNames[j]);
                try {
                    appinfo = GlobalInstance.pm.getApplicationInfo(packageNames[j], 0);
                    ShareItem item = new ShareItem(
                            GlobalInstance.pm.getApplicationIcon(appinfo),
                            GlobalInstance.pm.getApplicationLabel(appinfo).toString(),
                            packageNames[j],
                            shareClass[i]
                    );
                    list.add(item);
                    break;
                } catch (Exception e) {

                }
            }

        }
        adapter = new ShareAdapter(getActivity(), list);
        gvShare.setAdapter(adapter);
        UIUtils.makeGridViewFullSize(gvShare, UIUtils.dipToPx(80), 3);
    }

    @Override
    public void initEvents() {
        btnCancel.setOnClickListener(this);
        gvShare.setOnItemClickListener(this);
    }

    @Override
    public void initLogic() {

    }

    @Override
    public int getFragmentLayoutResId() {
        return R.layout.layout_share;
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
    public Bundle getFragmentState() {
        return null;
    }

    @Override
    public void onClick(View v) {
        switch (v.getId()) {
            case R.id.btnCancel:
                getActivity().finish();
                break;
        }

    }

    private Intent createShareIntent() {
        Intent shareIntent = new Intent(Intent.ACTION_SEND);
        shareIntent.setType("image/*");
        shareIntent.putExtra(Intent.EXTRA_TEXT, getString(R.string.share_body));
        shareIntent.putExtra(Intent.EXTRA_SUBJECT, getString(R.string.share_title));
        return shareIntent;
    }

    @Override
    public void onItemClick(AdapterView<?> parent, View view, int position, long id) {
        ShareItem item = list.get(position);
        // TODO: share to
    }
}
