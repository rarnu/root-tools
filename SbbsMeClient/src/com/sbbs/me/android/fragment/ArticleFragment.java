package com.sbbs.me.android.fragment;

import java.util.List;

import android.app.Activity;
import android.content.Intent;
import android.content.Loader;
import android.content.Loader.OnLoadCompleteListener;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.View.OnLongClickListener;
import android.widget.RelativeLayout;
import android.widget.TextView;
import android.widget.Toast;

import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.devlib.base.BaseSlidingActivity;
import com.rarnu.utils.ResourceUtils;
import com.sbbs.me.android.EditBlockActivity;
import com.sbbs.me.android.Global;
import com.sbbs.me.android.R;
import com.sbbs.me.android.UserDetailActivity;
import com.sbbs.me.android.api.SbbsMeAPI;
import com.sbbs.me.android.api.SbbsMeArticle;
import com.sbbs.me.android.api.SbbsMeBlock;
import com.sbbs.me.android.api.SbbsMeLogs;
import com.sbbs.me.android.api.SbbsMeSideBlocks;
import com.sbbs.me.android.component.BlockTextView;
import com.sbbs.me.android.consts.MenuIds;
import com.sbbs.me.android.dialog.ArticleMenuDialog;
import com.sbbs.me.android.dialog.ConfirmDialog;
import com.sbbs.me.android.dialog.ShareDialog;
import com.sbbs.me.android.loader.SbbsArticleLoader;
import com.sbbs.me.android.utils.Config;
import com.sbbs.me.android.utils.CustomUtils;

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
		SbbsMeAPI.writeLogT(getActivity(), SbbsMeLogs.LOG_ARTICLE, "");

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
		miShare.setShowAsAction(MenuItem.SHOW_AS_ACTION_IF_ROOM);
		miShare.setIcon(android.R.drawable.ic_menu_share);
	}

	@Override
	public boolean onOptionsItemSelected(MenuItem item) {
		switch (item.getItemId()) {
		case MenuIds.MENU_ID_SHARE:
			if (article != null) {
				String body = getString(R.string.share_fmt,
						article.main_block.Subject, article.main_block.Id);
				startActivity(new Intent(getActivity(), ShareDialog.class)
						.putExtra("body", body));
			}
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
		if (getActivity() != null) {
			if (article != null) {
				isMyArticle = article.main_block.AuthorId.equals(Config
						.getUserId(getActivity()));
				buildUI();
			} else {
				doArticleAndClose();
			}
			tvLoading.setVisibility(View.GONE);
		}
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
			SbbsMeSideBlocks sb = SbbsMeAPI.getSideBlocks(article,
					article.main_block.Id);
			CustomUtils.addBlock(getActivity(), article.main_block,
					sb.leftBlockCount, sb.rightBlockCount,
					article.users.get(article.main_block.AuthorId), layArticle,
					viewId, 100000, true, this, this);
			blockCount++;
			viewId++;
		}

		if (article.sub_blocks != null) {
			SbbsMeSideBlocks sb = null;
			for (int i = 0; i < article.sub_blocks.size(); i++) {
				sb = SbbsMeAPI.getSideBlocks(article,
						article.sub_blocks.get(i).Id);
				CustomUtils.addBlock(getActivity(), article.sub_blocks.get(i),
						sb.leftBlockCount, sb.rightBlockCount,
						article.users.get(article.sub_blocks.get(i).AuthorId),
						layArticle, viewId, 100000, false, this, this);
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

	@Override
	public boolean onLongClick(View v) {
		if (SbbsMeAPI.isLogin()) {
			final SbbsMeBlock item = ((BlockTextView) v).getBlock();
			startActivityForResult(
					new Intent(getActivity(), ArticleMenuDialog.class)
							.putExtra("item", item).putExtra("isMyArticle",
									isMyArticle), 0);
		} else {
			Toast.makeText(getActivity(), R.string.not_login, Toast.LENGTH_LONG)
					.show();
		}
		return true;
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

		SbbsMeAPI.writeLogT(getActivity(), SbbsMeLogs.LOG_BLOCK_DELETE, "");
	}

	@Override
	public void onClick(View v) {
		final SbbsMeBlock item = ((BlockTextView) v).getBlock();
		Global.passArticle = article;

		int touchPos = ((BlockTextView) v).getTouchedPosition();
		switch (touchPos) {
		case 0:
			if (blockHasLeftBlock(item.Id)) {
				// Bundle bn = new Bundle();
				// bn.putSerializable("item", item);
				// ((BaseFragment) getFragmentManager().findFragmentByTag(
				// getString(R.tag.tag_origin_post_fragment)))
				// .setNewArguments(bn);
				// ((BaseSlidingActivity) getActivity()).showMenu();
				getActivity().finish();
			}
			break;
		case 1:
			if (item.RightBlockCount != 0) {
				Bundle bn = new Bundle();
				bn.putSerializable("item", item);
				((BaseFragment) getFragmentManager().findFragmentByTag(
						getString(R.tag.tag_comment_fragment)))
						.setNewArguments(bn);
				((BaseSlidingActivity) getActivity()).showSecondaryMenu();
			}
			break;
		}
	}

	private boolean blockHasLeftBlock(String blockId) {
		boolean retBool = false;

		List<SbbsMeBlock> list = article.left_blocks.get(blockId);
		if (list != null && list.size() != 0) {
			retBool = true;
		}
		return retBool;
	}
}
