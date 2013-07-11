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
import android.widget.ListView;
import android.widget.TextView;

import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.utils.ResourceUtils;
import com.sbbs.me.android.ArticleActivity;
import com.sbbs.me.android.R;
import com.sbbs.me.android.adapter.SbbsMeArticleAdapter;
import com.sbbs.me.android.api.SbbsMeBlock;
import com.sbbs.me.android.api.SbbsMeTag;
import com.sbbs.me.android.loader.SbbsTagBlockListLoader;

public class TagArticleListFragment extends BaseFragment implements
		OnLoadCompleteListener<List<SbbsMeBlock>>, OnItemClickListener {

	ListView lvArticle;
	SbbsTagBlockListLoader loader;
	SbbsMeArticleAdapter adapter;
	TextView tvLoading;
	List<SbbsMeBlock> listArticle;
	SbbsMeTag tagItem;

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
		lvArticle = (ListView) innerView.findViewById(R.id.lvArticle);
		tvLoading = (TextView) innerView.findViewById(R.id.tvLoading);
		if (listArticle == null) {
			listArticle = new ArrayList<SbbsMeBlock>();
		}
		adapter = new SbbsMeArticleAdapter(getActivity(), listArticle);
		lvArticle.setAdapter(adapter);
		loader = new SbbsTagBlockListLoader(getActivity());
	}

	@Override
	public void initEvents() {
		lvArticle.setOnItemClickListener(this);
		loader.registerListener(0, this);
	}

	@Override
	public void initLogic() {
		tagItem = (SbbsMeTag) getArguments().getSerializable("item");
		if (listArticle.size() == 0) {
			tvLoading.setVisibility(View.VISIBLE);
			loader.setTagId(tagItem.Id);
			loader.startLoading();
		}
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
		listArticle.clear();
		if (data != null) {
			listArticle.addAll(data);
		}
		adapter.setNewList(listArticle);
		tvLoading.setVisibility(View.GONE);
	}

	@Override
	public void onItemClick(AdapterView<?> parent, View view, int position,
			long id) {
		final SbbsMeBlock item = listArticle.get(position);

		startActivityForResult(new Intent(getActivity(), ArticleActivity.class)
				.putExtra("articleId", item.Id).putExtra("item", item), 2);
	}

}
