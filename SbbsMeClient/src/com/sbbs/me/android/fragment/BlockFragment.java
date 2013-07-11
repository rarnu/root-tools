package com.sbbs.me.android.fragment;

import java.util.List;

import org.markdown4j.Markdown4jProcessor;

import android.app.Activity;
import android.content.Intent;
import android.content.Loader;
import android.content.Loader.OnLoadCompleteListener;
import android.os.Bundle;
import android.util.Log;
import android.view.Menu;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.LinearLayout;
import android.widget.RelativeLayout;
import android.widget.TextView;

import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.utils.ResourceUtils;
import com.rarnu.utils.UIUtils;
import com.sbbs.me.android.ArticleActivity;
import com.sbbs.me.android.Global;
import com.sbbs.me.android.R;
import com.sbbs.me.android.api.SbbsMeArticle;
import com.sbbs.me.android.api.SbbsMeBlock;
import com.sbbs.me.android.component.BlockTextView;
import com.sbbs.me.android.loader.SbbsArticleLoader;

public class BlockFragment extends BaseFragment implements OnClickListener,
		OnLoadCompleteListener<SbbsMeArticle> {

	String blockId;
	SbbsMeBlock block;
	SbbsMeArticle article;
	List<SbbsMeBlock> listComment;
	SbbsArticleLoader loader;

	RelativeLayout layBlock, layComment;
	TextView tvLoading;

	public BlockFragment() {
		super();
		tagText = ResourceUtils.getString(R.tag.tag_block_fragment);
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
		layBlock = (RelativeLayout) innerView.findViewById(R.id.layBlock);
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
		block = (SbbsMeBlock) getArguments().getSerializable("item");
		blockId = block.Id;
		article = Global.passArticle;
		loader.setArticleId(article.main_block.Id);
		Log.e("block", blockId);
		buildUI();
	}

	@Override
	public int getFragmentLayoutResId() {
		return R.layout.fragment_block;
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

	private void buildUI() {
		layBlock.removeAllViews();
		layComment.removeAllViews();
		listComment = article.right_blocks.get(blockId);

		addBlock(block, article.users.get(article.main_block.AuthorId), 100000,
				true, layBlock);
		if (listComment != null && listComment.size() != 0) {
			int viewId = 110000;
			for (int i = 0; i < listComment.size(); i++) {
				addBlock(listComment.get(i),
						article.users.get(listComment.get(i).AuthorId), viewId,
						true, layComment);
				viewId++;
			}
		}
	}

	private void addBlock(SbbsMeBlock item, String headUrl, int viewId,
			boolean needHead, RelativeLayout parent) {

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
			if (parent.getId() == R.id.layComment) {
				block.setOnClickListener(this);
			}

			parent.addView(block);
			parent.postInvalidate();
		}
	}

	@Override
	public void onClick(View v) {
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
		if (article != null) {
			buildUI();
		} else {
			doArticleAndClose();
		}
		tvLoading.setVisibility(View.GONE);
	}

	private void doArticleAndClose() {
		Global.autoRefreshTag = true;
		Global.autoLoadArticleTag = true;
		if (getActivity() != null) {
			getActivity().setResult(Activity.RESULT_OK);
			getActivity().finish();
		}
	}

}
