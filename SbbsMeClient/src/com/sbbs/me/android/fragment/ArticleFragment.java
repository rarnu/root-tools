package com.sbbs.me.android.fragment;

import android.os.Bundle;
import android.util.Log;
import android.view.Menu;

import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.utils.ResourceUtils;
import com.sbbs.me.android.R;

public class ArticleFragment extends BaseFragment {

	public ArticleFragment() {
		super();
		tagText = ResourceUtils.getString(R.tag.tag_article_fragment);
	}

	@Override
	public int getBarTitle() {
		return R.string.article_name;
	}

	@Override
	public int getBarTitleWithPath() {
		return R.string.article_name;
	}

	@Override
	public String getCustomTitle() {
		return null;
	}

	@Override
	public void initComponents() {

	}

	@Override
	public void initEvents() {

	}

	@Override
	public void initLogic() {
		String id = getArguments().getString("articleId");
		Log.e("initLogic", id);
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

}
