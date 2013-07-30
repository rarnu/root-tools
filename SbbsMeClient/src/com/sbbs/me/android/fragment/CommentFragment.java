package com.sbbs.me.android.fragment;

import java.util.List;

import android.content.Intent;
import android.content.Loader;
import android.content.Loader.OnLoadCompleteListener;
import android.os.Bundle;
import android.util.Log;
import android.view.Menu;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.RelativeLayout;
import android.widget.TextView;

import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.devlib.base.BaseSlidingActivity;
import com.rarnu.utils.ResourceUtils;
import com.sbbs.me.android.ArticleActivity;
import com.sbbs.me.android.Global;
import com.sbbs.me.android.R;
import com.sbbs.me.android.api.SbbsMeAPI;
import com.sbbs.me.android.api.SbbsMeArticle;
import com.sbbs.me.android.api.SbbsMeBlock;
import com.sbbs.me.android.api.SbbsMeSideBlocks;
import com.sbbs.me.android.component.BlockTextView;
import com.sbbs.me.android.loader.SbbsArticleLoader;
import com.sbbs.me.android.utils.CustomUtils;

public class CommentFragment extends BaseFragment implements OnClickListener,
		OnLoadCompleteListener<SbbsMeArticle> {

	String blockId;
	SbbsMeBlock block;
	SbbsMeArticle article;
	List<SbbsMeBlock> listComment;
	SbbsArticleLoader loader;

	RelativeLayout layComment;
	TextView tvLoading;

	public CommentFragment() {
		super();
		tagText = ResourceUtils.getString(R.tag.tag_comment_fragment);
	}

	@Override
	public int getBarTitle() {
		return R.string.block_name;
	}

	@Override
	public int getBarTitleWithPath() {
		return R.string.block_name;
	}

	@Override
	public String getCustomTitle() {
		String subject = "";
		if (block != null) {
			subject = block.Subject;
		}
		return subject;
	}

	@Override
	public void initComponents() {
		layComment = (RelativeLayout) innerView.findViewById(R.id.layComment);
		tvLoading = (TextView) innerView.findViewById(R.id.tvLoading);
		loader = new SbbsArticleLoader(getActivity());
	}

	@Override
	public void initEvents() {
		loader.registerListener(0, this);
	}

	@Override
	public void initLogic() {

	}

	@Override
	public int getFragmentLayoutResId() {
		return R.layout.fragment_comment;
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
		block = (SbbsMeBlock) bn.getSerializable("item");
		blockId = block.Id;
		article = Global.passArticle;
		loader.setArticleId(article.main_block.Id);
		Log.e("block", blockId);
		buildUI();
	}

	@Override
	public Bundle getFragmentState() {
		return null;
	}

	private void buildUI() {
		layComment.removeAllViews();
		listComment = article.right_blocks.get(blockId);

		if (listComment != null && listComment.size() != 0) {
			int viewId = 110000;
			SbbsMeSideBlocks sb = null;
			for (int i = 0; i < listComment.size(); i++) {
				sb = SbbsMeAPI.getSideBlocks(article, listComment.get(i).Id);
				CustomUtils.addBlock(getActivity(), listComment.get(i),
						sb.leftBlockCount, sb.rightBlockCount,
						article.users.get(listComment.get(i).AuthorId),
						layComment, viewId, 110000, true, this, null);

				viewId++;
			}
		}
	}

	@Override
	public void onClick(View v) {
		((BaseSlidingActivity) getActivity()).toggle();
		final SbbsMeBlock item = ((BlockTextView) v).getBlock();
		Log.e("onClick", String.format("right:%d", item.RightBlockCount));
		startActivityForResult(new Intent(getActivity(), ArticleActivity.class)
				.putExtra("articleId", item.Id).putExtra("item", item), 0);
	}

	@Override
	public void onActivityResult(int requestCode, int resultCode, Intent data) {
		switch (requestCode) {
		case 0: {
			if (Global.autoCommentRefreshTag) {
				Global.autoCommentRefreshTag = false;
				tvLoading.setVisibility(View.VISIBLE);
				loader.startLoading();
			}
		}
			break;
		}
	}

	@Override
	public void onLoadComplete(Loader<SbbsMeArticle> loader, SbbsMeArticle data) {
		article = data;
		if (getActivity() != null) {
			if (article != null) {
				buildUI();
			}
			tvLoading.setVisibility(View.GONE);
		}
	}

}
