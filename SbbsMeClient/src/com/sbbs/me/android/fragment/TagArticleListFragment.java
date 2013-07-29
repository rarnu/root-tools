package com.sbbs.me.android.fragment;

import java.util.ArrayList;
import java.util.List;

import android.content.Intent;
import android.content.Loader;
import android.content.Loader.OnLoadCompleteListener;
import android.os.Bundle;
import android.view.Menu;
import android.view.View;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemClickListener;
import android.widget.TextView;
import android.widget.Toast;

import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.devlib.component.PullDownListView;
import com.rarnu.devlib.component.intf.OnPullDownListener;
import com.rarnu.utils.ResourceUtils;
import com.rarnu.utils.UIUtils;
import com.sbbs.me.android.ArticleActivity;
import com.sbbs.me.android.R;
import com.sbbs.me.android.adapter.SbbsMeArticleAdapter;
import com.sbbs.me.android.api.SbbsMeBlock;
import com.sbbs.me.android.api.SbbsMeTag;
import com.sbbs.me.android.loader.SbbsTagBlockListLoader;

public class TagArticleListFragment extends BaseFragment implements
		OnLoadCompleteListener<List<SbbsMeBlock>>, OnItemClickListener,
		OnPullDownListener {

	PullDownListView lvArticle;
	SbbsTagBlockListLoader loader;
	SbbsMeArticleAdapter adapter;
	TextView tvLoading;
	List<SbbsMeBlock> listArticle;
	SbbsMeTag tagItem;

	int page = 1;
	private static final int PAGE_SIZE = 20;
	boolean isBottom = false;

	public TagArticleListFragment() {
		super();
		tagText = ResourceUtils.getString(R.tag.tag_article_list_fragment);
	}

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
		String tagName = "";
		if (tagItem != null) {
			tagName = tagItem.Name;
		}
		return tagName;
	}

	@Override
	public void initComponents() {
		lvArticle = (PullDownListView) innerView.findViewById(R.id.lvArticle);
		tvLoading = (TextView) innerView.findViewById(R.id.tvLoading);
		if (listArticle == null) {
			listArticle = new ArrayList<SbbsMeBlock>();
		}
		adapter = new SbbsMeArticleAdapter(getActivity(), listArticle);
		lvArticle.getListView().setAdapter(adapter);
		lvArticle.enableAutoFetchMore(true, 1);

		int devide = UIUtils.dipToPx(8);
		lvArticle.getListView().setDivider(null);
		lvArticle.getListView().setDividerHeight(devide);
		lvArticle.getListView().setPadding(devide, devide, devide, devide);
		lvArticle.getListView().setSelector(R.color.transparent);
		lvArticle.getListView().setOverScrollMode(View.OVER_SCROLL_NEVER);
		loader = new SbbsTagBlockListLoader(getActivity());
	}

	@Override
	public void initEvents() {
		lvArticle.setOnPullDownListener(this);
		lvArticle.getListView().setOnItemClickListener(this);
		loader.registerListener(0, this);
	}

	@Override
	public void initLogic() {
		tagItem = (SbbsMeTag) getArguments().getSerializable("item");
		page = 1;
		isBottom = false;

		tvLoading.setVisibility(View.VISIBLE);
		loader.setTagId(tagItem.Id);
		loader.setPage(page, PAGE_SIZE);
		loader.startLoading();
		lvArticle.notifyDidLoad();
	}

	@Override
	public int getFragmentLayoutResId() {
		return R.layout.fragment_tag_article_list;
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
	public void onLoadComplete(Loader<List<SbbsMeBlock>> loader,
			List<SbbsMeBlock> data) {
		if (page == 1) {
			listArticle.clear();
		}
		if (data != null && data.size() != 0) {
			listArticle.addAll(data);
		} else {
			isBottom = true;
		}
		if (getActivity() != null) {
			adapter.setNewList(listArticle);
			tvLoading.setVisibility(View.GONE);
			if (listArticle.size() == 0) {
				Toast.makeText(getActivity(), R.string.tag_no_article,
						Toast.LENGTH_LONG).show();
				getActivity().finish();
			}
			lvArticle.notifyDidRefresh();
			lvArticle.notifyDidMore();
		}
	}

	@Override
	public void onItemClick(AdapterView<?> parent, View view, int position,
			long id) {
		final SbbsMeBlock item = listArticle.get(position);

		startActivity(new Intent(getActivity(), ArticleActivity.class)
				.putExtra("articleId", item.Id).putExtra("item", item));
	}

	@Override
	public void onRefresh() {
		page = 1;
		isBottom = false;
		loader.setPage(page, PAGE_SIZE);
		loader.startLoading();
	}

	@Override
	public void onMore() {
		if (!isBottom) {
			page++;
			loader.setPage(page, PAGE_SIZE);
			loader.startLoading();
		} else {
			lvArticle.notifyDidMore();
		}

	}

}
