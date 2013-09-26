package com.rarnu.tools.root.fragment;

import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.text.Html;
import android.view.Menu;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.*;
import com.rarnu.devlib.base.BasePopupFragment;
import com.rarnu.devlib.component.DataBar;
import com.rarnu.tools.root.MainActivity;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.adapter.CustomCleanAdapter;
import com.rarnu.tools.root.common.CustomPackageInfo;
import com.rarnu.tools.root.utils.CustomPackageUtils;

import java.util.List;

public class CustomCleanManagerFragment extends BasePopupFragment implements OnClickListener {

    DataBar barCustomClean;
    ListView lvCustomClean;
    GridView gvCustomClean;
    TextView tvEmptyHint;
    CustomCleanAdapter adapter = null;
    Handler hSelectApp = new Handler() {
        @Override
        public void handleMessage(Message msg) {
            if (msg.what == 1) {
                showSelectedCount();
            }
            super.handleMessage(msg);
        }
    };

    @Override
    public int getBarTitle() {
        return R.string.custom_app_clean;
    }

    @Override
    public int getBarTitleWithPath() {
        return R.string.custom_app_clean;
    }

    @Override
    public void initComponents() {
        gvCustomClean = (GridView) innerView.findViewById(R.id.gvCustomClean);
        lvCustomClean = (ListView) innerView.findViewById(R.id.lvCustomClean);
        barCustomClean = (DataBar) innerView.findViewById(R.id.barCustomClean);
        tvEmptyHint = (TextView) innerView.findViewById(R.id.tvEmptyHint);
    }

    @Override
    public void initLogic() {
        loadCustomClean();
        showSelectedCount();
    }

    @Override
    public int getFragmentLayoutResId() {
        return R.layout.layout_custom_clean_manager;
    }

    private void loadCustomClean() {
        adapter = new CustomCleanAdapter(getActivity(), CustomPackageUtils.getCustomPackageList(), hSelectApp);
        if (gvCustomClean != null) {
            gvCustomClean.setAdapter(adapter);
        }
        if (lvCustomClean != null) {
            lvCustomClean.setAdapter(adapter);
        }

        tvEmptyHint.setText(Html.fromHtml(getString(R.string.custom_clean_empty)));
        tvEmptyHint.setVisibility(CustomPackageUtils.getCustomPackageList().size() == 0 ? View.VISIBLE : View.GONE);
    }

    private void deleteCustomClean() {
        int count = CustomPackageUtils.getCustomPackageList().size();
        for (int i = count - 1; i >= 0; i--) {
            if (CustomPackageUtils.getCustomPackageList().get(i).checked) {
                CustomPackageUtils.removeCustomPackage(i);
            }
        }
        adapter.notifyDataSetChanged();
        showSelectedCount();
        boolean saved = CustomPackageUtils.saveCustomPackages();
        if (!saved) {
            Toast.makeText(getActivity(), R.string.save_custom_clean_error, Toast.LENGTH_LONG).show();
        }
        tvEmptyHint.setVisibility(CustomPackageUtils.getCustomPackageList().size() == 0 ? View.VISIBLE : View.GONE);
    }

    private void showSelectedCount() {
        int count = getSelectedCount(CustomPackageUtils.getCustomPackageList());
        String cap = String.format(getResources()
                .getString(R.string.btn_delete), count);
        barCustomClean.setButton1Text(cap);
        barCustomClean.setVisibility(count == 0 ? View.GONE : View.VISIBLE);
    }

    private int getSelectedCount(List<CustomPackageInfo> list) {
        int count = 0;
        if (list != null) {
            for (int i = 0; i < list.size(); i++) {
                if (list.get(i).checked) {
                    count++;
                }
            }
        }
        return count;
    }

    private void setItemSelectedStatus(List<CustomPackageInfo> list, BaseAdapter adapter, Handler h, boolean selected) {
        for (int i = 0; i < list.size(); i++) {
            list.get(i).checked = selected;
        }
        adapter.notifyDataSetChanged();
        h.sendEmptyMessage(1);
    }

    @Override
    public void onClick(View v) {
        switch (v.getId()) {
            case R.id.barButton1:
                deleteCustomClean();
                break;
            case R.id.barButton2:
                setItemSelectedStatus(CustomPackageUtils.getCustomPackageList(), adapter, hSelectApp, false);
                break;
            case R.id.chkSelAll:
                boolean selected = barCustomClean.getCheckBox().isChecked();
                setItemSelectedStatus(CustomPackageUtils.getCustomPackageList(), adapter, hSelectApp, selected);
                break;
        }

    }

    @Override
    public void initMenu(Menu menu) {

    }

    @Override
    public void initEvents() {
        barCustomClean.getButton1().setOnClickListener(this);
        barCustomClean.getButton2().setOnClickListener(this);
        barCustomClean.getCheckBox().setOnClickListener(this);
    }

    @Override
    public String getMainActivityName() {
        return MainActivity.class.getName();
    }

    @Override
    public void onGetNewArguments(Bundle bn) {

    }

    @Override
    public String getCustomTitle() {
        return null;
    }

    @Override
    public Bundle getFragmentState() {
        return null;
    }

}
