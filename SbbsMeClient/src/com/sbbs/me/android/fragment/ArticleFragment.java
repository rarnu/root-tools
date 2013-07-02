package com.sbbs.me.android.fragment;

import org.markdown4j.Markdown4jProcessor;

import android.content.Intent;
import android.content.Loader;
import android.content.Loader.OnLoadCompleteListener;
import android.os.Bundle;
import android.text.Html;
import android.util.Log;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.LinearLayout;
import android.widget.RelativeLayout;
import android.widget.TextView;

import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.utils.ResourceUtils;
import com.rarnu.utils.UIUtils;
import com.sbbs.me.android.R;
import com.sbbs.me.android.api.SbbsMeArticle;
import com.sbbs.me.android.consts.MenuIds;
import com.sbbs.me.android.loader.SbbsArticleLoader;

public class ArticleFragment extends BaseFragment implements
		OnLoadCompleteListener<SbbsMeArticle> {

	RelativeLayout layArticle;
	SbbsArticleLoader loader;
	SbbsMeArticle article = null;
	TextView tvLoading;

	MenuItem miShare;

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
		layArticle = (RelativeLayout) innerView.findViewById(R.id.layArticle);
		tvLoading = (TextView) innerView.findViewById(R.id.tvLoading);
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

		tvLoading.setVisibility(View.VISIBLE);
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
		miShare = menu.add(0, MenuIds.MENU_ID_SHARE, 99, R.string.share);
		miShare.setShowAsAction(MenuItem.SHOW_AS_ACTION_ALWAYS);
		miShare.setIcon(android.R.drawable.ic_menu_share);
		//
		// ShareActionProvider actionProvider = new ShareActionProvider(
		// getActivity());
		// miShare.setActionProvider(actionProvider);
		// actionProvider
		// .setShareHistoryFileName(ShareActionProvider.DEFAULT_SHARE_HISTORY_FILE_NAME);
		// actionProvider.setShareIntent(createShareIntent());
	}

	private Intent createShareIntent() {
		Intent shareIntent = new Intent(Intent.ACTION_SEND);
		shareIntent.setType("image/*");
		// shareIntent.putExtra(Intent.EXTRA_TEXT, article.main_block.Subject);
		return shareIntent;
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
		tvLoading.setVisibility(View.GONE);
	}

	private void buildUI() {

		int viewId = 100000;

		if (article.main_block != null) {
			if (getActivity() != null && getActivity().getActionBar() != null) {
				getActivity().getActionBar().setTitle(
						article.main_block.Subject);
			}
			addTextView(article.main_block.Body, viewId,
					article.main_block.Format.equals("Markdown"));
			viewId++;
		}

		if (article.sub_blocks != null) {
			for (int i = 0; i < article.sub_blocks.size(); i++) {
				addTextView(article.sub_blocks.get(i).Body, viewId,
						article.sub_blocks.get(i).Format.equals("Markdown"));
				viewId++;
			}
		}

	}

	private void addTextView(String htmlText, int viewId, boolean isMarkdown) {
		Log.e("addTextView", htmlText);
		if (getActivity() != null) {
			TextView layMain = new TextView(getActivity());
			layMain.setId(viewId);
			RelativeLayout.LayoutParams rllp = new RelativeLayout.LayoutParams(
					LinearLayout.LayoutParams.MATCH_PARENT,
					LinearLayout.LayoutParams.WRAP_CONTENT);
			if (viewId > 100000) {
				rllp.addRule(RelativeLayout.BELOW, viewId - 1);
			}
			rllp.bottomMargin = UIUtils.dipToPx(4);
			layMain.setLayoutParams(rllp);
			try {
				layMain.setText(Html
						.fromHtml(isMarkdown ? (new Markdown4jProcessor()
								.process(htmlText)) : htmlText));

			} catch (Exception e) {
				Log.e("Mrkdown", e.getMessage());
			}
			layMain.setFocusable(true);
			layMain.setClickable(true);
			layMain.setTextSize(18);
			layMain.setTextColor(0xFF808080);

			layMain.setBackgroundResource(R.drawable.article_list_selector);
			layArticle.addView(layMain);
			layArticle.postInvalidate();
		}
	}
}
