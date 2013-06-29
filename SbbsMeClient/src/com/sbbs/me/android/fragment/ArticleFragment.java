package com.sbbs.me.android.fragment;

import android.content.Loader;
import android.content.Loader.OnLoadCompleteListener;
import android.os.Bundle;
import android.text.Html;
import android.util.Log;
import android.view.Menu;
import android.widget.LinearLayout;
import android.widget.TextView;

import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.utils.ResourceUtils;
import com.sbbs.me.android.R;
import com.sbbs.me.android.api.SbbsMeArticle;
import com.sbbs.me.android.loader.SbbsArticleLoader;

public class ArticleFragment extends BaseFragment implements
		OnLoadCompleteListener<SbbsMeArticle> {

	LinearLayout layArticle;
	SbbsArticleLoader loader;
	SbbsMeArticle article = null;

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
		layArticle = (LinearLayout) innerView.findViewById(R.id.layArticle);

		loader = new SbbsArticleLoader(getActivity());
	}

	@Override
	public void initEvents() {
		loader.registerListener(0, this);
	}

	@Override
	public void initLogic() {
		String id = getArguments().getString("articleId");
		Log.e("initLogic", id);

		loader.setArticleId(id);
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
	public void onLoadComplete(Loader<SbbsMeArticle> loader, SbbsMeArticle data) {
		article = data;
		if (article != null) {
			buildUI();
		}
	}

	private void buildUI() {
		if (article.main_block != null) {
			addTextView(article.main_block.Body);
		}

		if (article.sub_blocks != null) {
			for (int i = 0; i < article.sub_blocks.size(); i++) {
				addTextView(article.sub_blocks.get(i).Body);
			}
		}

	}

	private void addTextView(String htmlText) {
		TextView layMain = new TextView(getActivity());
		layMain.setLayoutParams(new LinearLayout.LayoutParams(
				LinearLayout.LayoutParams.MATCH_PARENT,
				LinearLayout.LayoutParams.WRAP_CONTENT));
		layMain.setText(Html.fromHtml(htmlText));
		layArticle.addView(layMain);
	}
}
