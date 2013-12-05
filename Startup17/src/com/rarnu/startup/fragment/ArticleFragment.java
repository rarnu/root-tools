package com.rarnu.startup.fragment;

import android.content.Loader;
import android.os.Bundle;
import android.view.Menu;
import android.widget.TextView;
import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.startup.R;
import com.rarnu.startup.loader.ArticleLoader;
import com.rarnu.startup.pojo.ArticleItem;

public class ArticleFragment extends BaseFragment implements Loader.OnLoadCompleteListener<ArticleItem> {

    ArticleLoader loader;
    ArticleItem item;

    TextView tvDesc;

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
        tvDesc = (TextView) innerView.findViewById(R.id.tvDesc);
        loader = new ArticleLoader(getActivity());

    }

    @Override
    public void initEvents() {
        loader.registerListener(0, this);
    }

    @Override
    public void initLogic() {
        long id = getArguments().getLong("id");
        loader.setId(id);
        loader.startLoading();
    }

    @Override
    public int getFragmentLayoutResId() {
        return R.layout.fragment_article;
    }

    @Override
    public String getMainActivityName() {
        return "";
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
    public void onLoadComplete(Loader<ArticleItem> loader, ArticleItem data) {
        item = data;
        if (item != null) {
            if (getActivity() != null) {
                getActivity().getActionBar().setTitle(item.name);
                tvDesc.setText(item.desc);
            }
        }
    }
}
