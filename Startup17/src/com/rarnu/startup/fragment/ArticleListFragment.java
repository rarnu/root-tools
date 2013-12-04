package com.rarnu.startup.fragment;

import android.content.Intent;
import android.content.Loader;
import android.os.Bundle;
import android.util.Log;
import android.view.Menu;
import android.view.View;
import android.widget.AdapterView;
import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.devlib.component.PullDownListView;
import com.rarnu.devlib.component.intf.OnPullDownListener;
import com.rarnu.startup.ArticleActivity;
import com.rarnu.startup.R;
import com.rarnu.startup.adapter.ArticleListAdapter;
import com.rarnu.startup.loader.ArticleListLoader;
import com.rarnu.startup.pojo.ArticleListItem;
import com.rarnu.utils.ResourceUtils;

import java.util.ArrayList;
import java.util.List;

public class ArticleListFragment extends BaseFragment implements AdapterView.OnItemClickListener, OnPullDownListener, Loader.OnLoadCompleteListener<List<ArticleListItem>> {

    PullDownListView lvPulldown;
    ArticleListAdapter adapter;
    List<ArticleListItem> list;
    ArticleListLoader loader;
    int categoryId = 0;
    int page = 1;
    int pageSize = 20;
    boolean isEnd = false;
    private int type = 0;
    private int[] tags = new int[]{
            R.string.tag_0, R.string.tag_1, R.string.tag_2,
            R.string.tag_3, R.string.tag_4, R.string.tag_5,
            R.string.tag_6, R.string.tag_7, R.string.tag_8
    };
    private int[] titles = new int[]{
            R.string.title_0, R.string.title_1, R.string.title_2,
            R.string.title_3, R.string.title_4, R.string.title_5,
            R.string.title_6, R.string.title_7, R.string.title_8
    };

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
    public void initComponents() {
        lvPulldown = (PullDownListView) innerView.findViewById(R.id.lvPulldown);
        list = new ArrayList<ArticleListItem>();
        adapter = new ArticleListAdapter(getActivity(), list);
        lvPulldown.getListView().setAdapter(adapter);
        loader = new ArticleListLoader(getActivity());
        lvPulldown.enableAutoFetchMore(true, 1);
        lvPulldown.getListView().setFocusableInTouchMode(false);

    }

    @Override
    public void initEvents() {
        lvPulldown.getListView().setOnItemClickListener(this);
        lvPulldown.setOnPullDownListener(this);
        loader.registerListener(0, this);
    }

    @Override
    public void initLogic() {
        page = 1;
        setIsEnd(false);
        loader.setData(categoryId, page, pageSize);
        loader.startLoading();
        lvPulldown.notifyDidLoad();
    }

    @Override
    public int getFragmentLayoutResId() {
        return R.layout.fragment_article_list;
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

    public void setType(int type) {
        this.type = type;
        categoryId = type;
        tagText = ResourceUtils.getString(tags[type]);
        tabTitle = ResourceUtils.getString(titles[type]);
    }

    @Override
    public void onItemClick(AdapterView<?> parent, View view, int position, long id) {
        long articleId = list.get(position).id;
        Intent inArticle = new Intent(getActivity(), ArticleActivity.class);
        inArticle.putExtra("id", articleId);
        startActivity(inArticle);
    }

    @Override
    public void onRefresh() {
        page = 1;
        setIsEnd(false);
        loader.setData(categoryId, page, pageSize);
        loader.startLoading();
    }

    @Override
    public void onMore() {
        if (!isEnd) {
            page++;
            loader.setData(categoryId, page, pageSize);
            loader.startLoading();
        } else {
            lvPulldown.notifyDidMore();
        }
    }

    @Override
    public void onLoadComplete(Loader<List<ArticleListItem>> loader, List<ArticleListItem> data) {
        if (page == 1) {
            list.clear();
        }
        if (data != null) {
            Log.e("onLoadComplete", "add all: " + data.size());
            list.addAll(data);
        } else {
            setIsEnd(true);
        }
        if (getActivity() != null) {
            adapter.setNewList(list);
            lvPulldown.notifyDidRefresh();
            lvPulldown.notifyDidMore();
        }
    }

    private void setIsEnd(boolean end) {
        isEnd = end;
        if (isEnd) {
            lvPulldown.enableAutoFetchMore(false, 0);
            lvPulldown.showAutoFetchMore(false);
        } else {
            lvPulldown.enableAutoFetchMore(true, 1);
            lvPulldown.showAutoFetchMore(true);
        }
    }
}
