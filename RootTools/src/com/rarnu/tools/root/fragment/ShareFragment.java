package com.rarnu.tools.root.fragment;

import android.content.Intent;
import android.content.Loader;
import android.os.Bundle;
import android.view.Menu;
import android.view.View;
import android.widget.AdapterView;
import android.widget.GridView;
import android.widget.ImageView;
import android.widget.TextView;
import com.rarnu.devlib.base.BaseDialogFragment;
import com.rarnu.tools.root.MainActivity;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.adapter.ShareAdapter;
import com.rarnu.tools.root.common.ShareItem;
import com.rarnu.tools.root.loader.ShareLoader;
import com.rarnu.tools.root.utils.ShareUtils;
import com.rarnu.utils.UIUtils;

import java.util.ArrayList;
import java.util.List;

public class ShareFragment extends BaseDialogFragment implements View.OnClickListener, AdapterView.OnItemClickListener, Loader.OnLoadCompleteListener<List<ShareItem>> {

    ImageView btnCancel;
    GridView gvShare;
    List<ShareItem> list;
    ShareAdapter adapter;
    TextView tvLoading;

    String[] sharePackage = null;
    String[] shareClass = null;
    String[] shareSystem = null;

    ShareLoader loader;

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
        tvLoading = (TextView) innerView.findViewById(R.id.tvLoading);

        sharePackage = getResources().getStringArray(R.array.array_share_list);
        shareClass = getResources().getStringArray(R.array.array_share_class);
        shareSystem = getResources().getStringArray(R.array.array_share_system);

        loader = new ShareLoader(getActivity());
        list = new ArrayList<ShareItem>();
        adapter = new ShareAdapter(getActivity(), list);
        gvShare.setAdapter(adapter);

    }

    @Override
    public void initEvents() {
        btnCancel.setOnClickListener(this);
        gvShare.setOnItemClickListener(this);
        loader.registerListener(0, this);
    }

    @Override
    public void initLogic() {
        doStartLoad(false, true);
    }

    public void doStartLoad(boolean refresh, boolean showLoading) {
        tvLoading.setVisibility(showLoading?View.VISIBLE:View.GONE);
        loader.setData(sharePackage, shareClass, shareSystem);
        loader.setRefresh(refresh);
        loader.startLoading();
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
        for (int i = 0; i < sharePackage.length; i++) {
            if (item.packageName.matches(sharePackage[i])) {
                ShareUtils.share(getActivity(), i, item);
            }
        }
        getActivity().finish();

    }

    @Override
    public void onLoadComplete(Loader<List<ShareItem>> loader, List<ShareItem> data) {
        list.clear();
        if (data != null) {
            list.addAll(data);
        }
        if (getActivity() != null) {
            adapter.setNewList(list);
            UIUtils.makeGridViewFullSize(gvShare, UIUtils.dipToPx(80), 3);
            tvLoading.setVisibility(View.GONE);
            if (!ShareFragment.this.loader.isRefresh()) {
                doStartLoad(true, false);
            }
        }

    }
}
