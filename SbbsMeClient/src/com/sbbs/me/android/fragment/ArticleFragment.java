package com.sbbs.me.android.fragment;

import org.markdown4j.Markdown4jProcessor;

import android.app.Activity;
import android.content.Intent;
import android.content.Loader;
import android.content.Loader.OnLoadCompleteListener;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.util.Log;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.View.OnLongClickListener;
import android.widget.LinearLayout;
import android.widget.RelativeLayout;
import android.widget.TextView;

import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.devlib.base.BaseSlidingActivity;
import com.rarnu.utils.ResourceUtils;
import com.rarnu.utils.UIUtils;
import com.sbbs.me.android.EditBlockActivity;
import com.sbbs.me.android.Global;
import com.sbbs.me.android.R;
import com.sbbs.me.android.UserDetailActivity;
import com.sbbs.me.android.api.SbbsMeAPI;
import com.sbbs.me.android.api.SbbsMeArticle;
import com.sbbs.me.android.api.SbbsMeBlock;
import com.sbbs.me.android.component.BlockTextView;
import com.sbbs.me.android.consts.MenuIds;
import com.sbbs.me.android.dialog.ArticleMenuDialog;
import com.sbbs.me.android.dialog.ConfirmDialog;
import com.sbbs.me.android.loader.SbbsArticleLoader;
import com.sbbs.me.android.utils.Config;

public class ArticleFragment extends BaseFragment implements
		OnLoadCompleteListener<SbbsMeArticle>, OnLongClickListener,
		OnClickListener {

	RelativeLayout layArticle;
	SbbsArticleLoader loader;
	SbbsMeArticle article = null;
	TextView tvLoading;

	MenuItem miShare;
	SbbsMeBlock shareBlock;

	boolean isMyArticle = false;

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
		shareBlock = (SbbsMeBlock) getArguments().getSerializable("item");

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
	}

	@Override
	public boolean onOptionsItemSelected(MenuItem item) {
		switch (item.getItemId()) {
		case MenuIds.MENU_ID_SHARE:
			// TODO: share
			break;
		}
		return true;
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
			isMyArticle = article.main_block.AuthorId.equals(Config
					.getUserId(getActivity()));
			Log.e("isMyArticle", (isMyArticle ? "TRUE" : "FALSE"));
			buildUI();
		} else {
			doArticleAndClose();
		}
		tvLoading.setVisibility(View.GONE);
	}

	private void buildUI() {
		int blockCount = 0;
		int viewId = 100000;
		layArticle.removeAllViews();

		if (article.main_block != null) {
			if (getActivity() != null && getActivity().getActionBar() != null) {
				getActivity().getActionBar().setTitle(
						article.main_block.Subject);
			}

			addBlock(article.main_block,
					article.users.get(article.main_block.AuthorId), viewId,
					true);
			blockCount++;
			viewId++;
		}

		if (article.sub_blocks != null) {
			for (int i = 0; i < article.sub_blocks.size(); i++) {
				addBlock(article.sub_blocks.get(i),
						article.users.get(article.sub_blocks.get(i).AuthorId),
						viewId, false);
				blockCount++;
				viewId++;
			}
		}
		if (blockCount == 0) {
			doArticleAndClose();
		}

	}

	private void doArticleAndClose() {
		Global.autoRefreshTag = true;
		if (getActivity() != null) {
			getActivity().setResult(Activity.RESULT_OK);
			getActivity().finish();
		}
	}

	private void addBlock(SbbsMeBlock item, String headUrl, int viewId,
			boolean needHead) {

		String userId = item.AuthorId;
		String htmlText = item.Body;
		boolean isMarkdown = item.Format.equals("Markdown");

		if (getActivity() != null) {
			BlockTextView block = new BlockTextView(getActivity());
			block.setId(viewId);
			RelativeLayout.LayoutParams rllp = new RelativeLayout.LayoutParams(
					LinearLayout.LayoutParams.MATCH_PARENT,
					LinearLayout.LayoutParams.WRAP_CONTENT);
			if (viewId > 100000) {
				rllp.addRule(RelativeLayout.BELOW, viewId - 1);
			}
			rllp.bottomMargin = UIUtils.dipToPx(4);
			block.setLayoutParams(rllp);
			try {
				block.setText(isMarkdown ? (new Markdown4jProcessor()
						.process(htmlText)) : htmlText);
				if (needHead) {
					block.setHeadImageUrl(userId, headUrl);
				}
			} catch (Exception e) {

			}
			block.setBlock(item);
			block.setOnLongClickListener(this);
			block.setOnClickListener(this);

			layArticle.addView(block);
			layArticle.postInvalidate();
		}
	}

	@Override
	public boolean onLongClick(View v) {
		if (SbbsMeAPI.isLogin()) {
			final SbbsMeBlock item = ((BlockTextView) v).getBlock();
			startActivityForResult(
					new Intent(getActivity(), ArticleMenuDialog.class)
							.putExtra("item", item).putExtra("isMyArticle",
									isMyArticle), 0);
		}
		return false;
	}

	@Override
	public void onActivityResult(int requestCode, int resultCode, Intent data) {
		if (resultCode != Activity.RESULT_OK) {
			return;
		}
		switch (requestCode) {
		case 0: {
			// action selection callback
			SbbsMeBlock item = (SbbsMeBlock) data.getSerializableExtra("item");
			int mode = data.getIntExtra("mode", -1);
			if (mode != -1) {
				switch (mode) {
				case 3:
					startActivityForResult(
							new Intent(getActivity(), ConfirmDialog.class)
									.putExtra("item", item)
									.putExtra("ok", true)
									.putExtra("cancel", true)
									.putExtra("text",
											getString(R.string.confirm_delete)),
							2);
					break;
				case 4:
					startActivity(new Intent(getActivity(),
							UserDetailActivity.class).putExtra("user",
							item.AuthorId));
					break;
				default:
					startActivityForResult(new Intent(getActivity(),
							EditBlockActivity.class).putExtra("item", item)
							.putExtra("mode", mode), 1);
					break;
				}
			}

		}
			break;
		case 1: {
			// edit block callback
			Global.autoCommentRefreshTag = true;
			tvLoading.setVisibility(View.VISIBLE);
			loader.startLoading();
		}
			break;
		case 2: {
			// delete block callback
			Global.autoCommentRefreshTag = true;
			SbbsMeBlock item = (SbbsMeBlock) data.getSerializableExtra("item");
			deleteBlock(item.Id);
		}
			break;
		case 3: {
			// view block callback
			if (Global.autoLoadArticleTag) {
				Global.autoLoadArticleTag = false;
				tvLoading.setVisibility(View.VISIBLE);
				loader.startLoading();
			}
		}
			break;
		}
	}

	final Handler hDelete = new Handler() {
		@Override
		public void handleMessage(Message msg) {
			if (msg.what == 1) {
				String ret = (String) msg.obj;
				if (ret.contains("OK")) {
					tvLoading.setText(R.string.loading);
					loader.startLoading();
				} else {
					tvLoading.setVisibility(View.GONE);
				}
			}
			super.handleMessage(msg);
		};
	};

	private void deleteBlock(final String blockId) {
		tvLoading.setText(R.string.deleting);
		tvLoading.setVisibility(View.VISIBLE);
		new Thread(new Runnable() {

			@Override
			public void run() {
				Message msg = new Message();
				msg.what = 1;
				msg.obj = SbbsMeAPI.deleteBlock(blockId);
				hDelete.sendMessage(msg);
			}
		}).start();
	}

	@Override
	public void onClick(View v) {
		final SbbsMeBlock item = ((BlockTextView) v).getBlock();
		if (item.RightBlockCount != 0) {
			Global.passArticle = article;
			Bundle bn = new Bundle();
			bn.putSerializable("item", item);
			((BaseFragment) getFragmentManager().findFragmentByTag(
					getString(R.tag.tag_comment_fragment))).setNewArguments(bn);
			((BaseSlidingActivity) getActivity()).showSecondaryMenu();
		}
	}
}
