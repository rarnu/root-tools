package com.sbbs.me.android.fragment;

import java.util.List;

import org.markdown4j.Markdown4jProcessor;

import android.app.Activity;
import android.content.Intent;
import android.os.Bundle;
import android.util.Log;
import android.view.Menu;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.View.OnLongClickListener;
import android.widget.LinearLayout;
import android.widget.RelativeLayout;
import android.widget.TextView;

import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.utils.ResourceUtils;
import com.rarnu.utils.UIUtils;
import com.sbbs.me.android.BlockActivity;
import com.sbbs.me.android.EditBlockActivity;
import com.sbbs.me.android.Global;
import com.sbbs.me.android.R;
import com.sbbs.me.android.UserDetailActivity;
import com.sbbs.me.android.api.SbbsMeAPI;
import com.sbbs.me.android.api.SbbsMeArticle;
import com.sbbs.me.android.api.SbbsMeBlock;
import com.sbbs.me.android.component.BlockTextView;
import com.sbbs.me.android.dialog.ArticleMenuDialog;
import com.sbbs.me.android.dialog.ConfirmDialog;
import com.sbbs.me.android.utils.Config;

public class BlockFragment extends BaseFragment implements OnLongClickListener,
		OnClickListener {

	String blockId;
	SbbsMeBlock block;
	SbbsMeArticle article;
	List<SbbsMeBlock> listComment;

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
	}

	@Override
	public void initEvents() {

	}

	@Override
	public void initLogic() {
		block = (SbbsMeBlock) getArguments().getSerializable("item");
		blockId = block.Id;
		article = Global.passArticle;
		Log.e("block", blockId);

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
			block.setOnLongClickListener(this);

			if (parent.getId() == R.id.layComment) {
				block.setOnClickListener(this);
			}

			parent.addView(block);
			parent.postInvalidate();
		}
	}

	@Override
	public boolean onLongClick(View v) {
		if (SbbsMeAPI.isLogin()) {
			final SbbsMeBlock item = ((BlockTextView) v).getBlock();
			boolean isMyArticle = Config.getUserId(getActivity()).equals(
					item.AuthorId);
			startActivityForResult(
					new Intent(getActivity(), ArticleMenuDialog.class)
							.putExtra("item", item).putExtra("isMyArticle",
									isMyArticle), 0);
		}
		return false;
	}

	@Override
	public void onClick(View v) {
		final SbbsMeBlock item = ((BlockTextView) v).getBlock();
		Global.passArticle = article;
		startActivityForResult(
				new Intent(getActivity(), BlockActivity.class).putExtra("item",
						item), 3);

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
			// TODO: edit block callback

		}
			break;
		case 2: {
			// TODO: delete block callback
			SbbsMeBlock item = (SbbsMeBlock) data.getSerializableExtra("item");

		}
			break;
		case 3: {
			// TODO: more block callback

		}
			break;
		}
	}

}
