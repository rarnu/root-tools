package com.sbbs.me.android.fragment;

import android.app.Activity;
import android.content.Intent;
import android.os.Bundle;
import android.view.Menu;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.ImageView;
import android.widget.RelativeLayout;
import android.widget.TextView;

import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.utils.ResourceUtils;
import com.sbbs.me.android.R;

public class ArticleMenuFragment extends BaseFragment implements
		OnClickListener {

	ImageView ivCloseDialog;
	RelativeLayout btnAppendBlock, btnCommentBlock, btnEditBlock,
			btnDeleteBlock;
	TextView tvAppendBlock, tvCommentBlock, tvEditBlock, tvDeleteBlock;

	public ArticleMenuFragment() {
		super();
		tagText = ResourceUtils.getString(R.tag.tag_article_menu_fragment);
	}

	@Override
	public int getBarTitle() {
		return R.string.article_menu;
	}

	@Override
	public int getBarTitleWithPath() {
		return R.string.article_menu;
	}

	@Override
	public String getCustomTitle() {
		return null;
	}

	@Override
	public void initComponents() {
		btnAppendBlock = (RelativeLayout) innerView
				.findViewById(R.id.btnAppendBlock);
		btnCommentBlock = (RelativeLayout) innerView
				.findViewById(R.id.btnCommentBlock);
		btnEditBlock = (RelativeLayout) innerView
				.findViewById(R.id.btnEditBlock);
		btnDeleteBlock = (RelativeLayout) innerView
				.findViewById(R.id.btnDeleteBlock);
		ivCloseDialog = (ImageView) innerView.findViewById(R.id.ivCloseDialog);
		tvAppendBlock = (TextView) innerView.findViewById(R.id.tvAppendBlock);
		tvCommentBlock = (TextView) innerView.findViewById(R.id.tvCommentBlock);
		tvEditBlock = (TextView) innerView.findViewById(R.id.tvEditBlock);
		tvDeleteBlock = (TextView) innerView.findViewById(R.id.tvDeleteBlock);

		boolean isMyArticle = getArguments().getBoolean("isMyArticle", false);
		btnAppendBlock.setEnabled(isMyArticle);
		btnEditBlock.setEnabled(isMyArticle);
		btnDeleteBlock.setEnabled(isMyArticle);

		if (!isMyArticle) {
			tvAppendBlock.setTextColor(getResources().getColor(
					R.color.lightgrey));
			tvEditBlock
					.setTextColor(getResources().getColor(R.color.lightgrey));
			tvDeleteBlock.setTextColor(getResources().getColor(
					R.color.lightgrey));
		}

	}

	@Override
	public void initEvents() {
		btnAppendBlock.setOnClickListener(this);
		btnCommentBlock.setOnClickListener(this);
		btnEditBlock.setOnClickListener(this);
		btnDeleteBlock.setOnClickListener(this);
		ivCloseDialog.setOnClickListener(this);
	}

	@Override
	public void initLogic() {

	}

	@Override
	public int getFragmentLayoutResId() {
		return R.layout.dialog_article_menu;
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
	public void onClick(View v) {
		Intent inRet = new Intent();
		inRet.putExtra("item", getArguments().getSerializable("item"));
		switch (v.getId()) {
		case R.id.btnAppendBlock:
			inRet.putExtra("mode", 0);
			break;
		case R.id.btnCommentBlock:
			inRet.putExtra("mode", 1);
			break;
		case R.id.btnEditBlock:
			inRet.putExtra("mode", 2);
			break;
		case R.id.btnDeleteBlock:
			inRet.putExtra("mode", 3);
			break;
		case R.id.ivCloseDialog:
			getActivity().finish();
			return;
		}
		getActivity().setResult(Activity.RESULT_OK, inRet);
		getActivity().finish();
	}

}
