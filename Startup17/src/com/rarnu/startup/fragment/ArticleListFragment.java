package com.rarnu.startup.fragment;

import android.content.Intent;
import android.content.Loader;
import android.os.Bundle;
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

    PullDownListView lvPulldown;
    ArticleListAdapter adapter;
    List<ArticleListItem> list;
    ArticleListLoader loader;

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
        new Thread(new Runnable() {
            @Override
            public void run() {
                try {
                    Thread.sleep(500);
                    lvPulldown.notifyDidRefresh();
                } catch (Exception e) {

                }
            }
        }).start();
    }

    @Override
    public void onMore() {
        new Thread(new Runnable() {
            @Override
            public void run() {
                try {
                    Thread.sleep(500);
                    lvPulldown.notifyDidMore();
                } catch (Exception e) {

                }
            }
        }).start();
    }

    @Override
    public void onLoadComplete(Loader<List<ArticleListItem>> loader, List<ArticleListItem> data) {
        list.clear();
        if (data != null) {
            list.addAll(data);
        }
        if (getActivity() != null) {
            adapter.setNewList(list);
        }
    }
}
