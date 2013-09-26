package com.rarnu.tools.root.fragment;

import android.content.Loader;
import android.content.Loader.OnLoadCompleteListener;
import android.os.Bundle;
import android.view.Menu;
import android.view.View;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemClickListener;
import android.widget.ListView;
import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.devlib.component.DataProgressBar;
import com.rarnu.tools.root.MainActivity;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.adapter.RecommandAdapter;
import com.rarnu.tools.root.common.RecommandInfo;
import com.rarnu.tools.root.loader.RecommandLoader;
import com.rarnu.tools.root.utils.ApkUtils;

import java.util.ArrayList;
import java.util.List;

public class RecommandFragment extends BaseFragment implements OnLoadCompleteListener<List<RecommandInfo>>, OnItemClickListener {

    ListView lvRecommand;
    DataProgressBar progressRecommand;
    List<RecommandInfo> lstRecommand = new ArrayList<RecommandInfo>();
    RecommandAdapter adapter = null;
    RecommandLoader loader = null;

    @Override
    public int getBarTitle() {
        return R.string.short_recommand;
    }

    @Override
    public int getBarTitleWithPath() {
        return R.string.short_recommand_with_path;
    }

    @Override
    public void initComponents() {
        lvRecommand = (ListView) innerView.findViewById(R.id.lvRecommand);
        progressRecommand = (DataProgressBar) innerView.findViewById(R.id.progressRecommand);
        adapter = new RecommandAdapter(getActivity(), lstRecommand);
        lvRecommand.setAdapter(adapter);
        loader = new RecommandLoader(getActivity());
    }

    @Override
    public void initLogic() {
        doStartLoad();
    }

    @Override
    public int getFragmentLayoutResId() {
        return R.layout.layout_recommand;
    }

    @Override
    public void initMenu(Menu menu) {

    }

    private void doStartLoad() {
        progressRecommand.setAppName(getString(R.string.loading));
        progressRecommand.setVisibility(View.VISIBLE);
        loader.startLoading();
    }

    @Override
    public void onLoadComplete(Loader<List<RecommandInfo>> loader, List<RecommandInfo> data) {
        lstRecommand.clear();
        if (data != null) {
            lstRecommand.addAll(data);
        }
        adapter.setNewList(lstRecommand);
        progressRecommand.setVisibility(View.GONE);
    }

    @Override
    public void onItemClick(AdapterView<?> parent, View view, int position, long id) {
        RecommandInfo item = lstRecommand.get(position);
        ApkUtils.gotoApp(getActivity(), item.packageName, item.downloadUrl);

    }

    @Override
    public void initEvents() {
        lvRecommand.setOnItemClickListener(this);
        loader.registerListener(0, this);
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
