package com.sbbs.me.android.fragment;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.markdown4j.Markdown4jProcessor;

import android.app.Activity;
import android.content.Intent;
import android.net.Uri;
import android.os.Bundle;
import android.view.Menu;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemClickListener;
import android.widget.ImageView;
import android.widget.ListView;
import android.widget.RelativeLayout;
import android.widget.TextView;

import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.utils.ResourceUtils;
import com.sbbs.me.android.BigPictureActivity;
import com.sbbs.me.android.R;
import com.sbbs.me.android.adapter.SbbsMeArticleMenuObjectAdapter;
import com.sbbs.me.android.api.SbbsMeAPI;
import com.sbbs.me.android.api.SbbsMeArticleObject;
import com.sbbs.me.android.api.SbbsMeBlock;

public class ArticleMenuFragment extends BaseFragment implements
		OnClickListener, OnItemClickListener {

	ImageView ivCloseDialog;
	RelativeLayout btnAppendBlock, btnCommentBlock, btnEditBlock,
			btnDeleteBlock, btnViewUser;
	TextView tvAppendBlock, tvCommentBlock, tvEditBlock, tvDeleteBlock;
	ListView lvObject;
	SbbsMeArticleMenuObjectAdapter adapter;
	List<SbbsMeArticleObject> list;
	TextView tvNoObject;

	RelativeLayout layObject;

	public ArticleMenuFragment() {
		super();
		tagText = ResourceUtils.getString(R.string.tag_article_menu_fragment);
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
		btnViewUser = (RelativeLayout) innerView.findViewById(R.id.btnViewUser);
		ivCloseDialog = (ImageView) innerView.findViewById(R.id.ivCloseDialog);
		tvAppendBlock = (TextView) innerView.findViewById(R.id.tvAppendBlock);
		tvCommentBlock = (TextView) innerView.findViewById(R.id.tvCommentBlock);
		tvEditBlock = (TextView) innerView.findViewById(R.id.tvEditBlock);
		tvDeleteBlock = (TextView) innerView.findViewById(R.id.tvDeleteBlock);
		lvObject = (ListView) innerView.findViewById(R.id.lvObject);
		tvNoObject = (TextView) innerView.findViewById(R.id.tvNoObject);
		layObject = (RelativeLayout) innerView.findViewById(R.id.layObject);

		list = new ArrayList<SbbsMeArticleObject>();
		adapter = new SbbsMeArticleMenuObjectAdapter(getActivity(), list);
		lvObject.setAdapter(adapter);

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
		btnViewUser.setOnClickListener(this);
		ivCloseDialog.setOnClickListener(this);
		lvObject.setOnItemClickListener(this);
	}

	@Override
	public void initLogic() {
		SbbsMeBlock item = (SbbsMeBlock) getArguments().getSerializable("item");
		boolean isMarkdown = item.Format.equals("Markdown");
		String body = item.Body;
		if (isMarkdown) {
			try {
				body = new Markdown4jProcessor().process(item.Body);
			} catch (IOException e) {
			}
		}
		list = SbbsMeAPI.getArticleObjects(body);
		if (list != null) {
			adapter.setNewList(list);
		}
		tvNoObject
				.setVisibility((list == null || list.size() == 0) ? View.VISIBLE
						: View.GONE);
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
		case R.id.btnViewUser:
			inRet.putExtra("mode", 4);
			break;
		case R.id.ivCloseDialog:
			getActivity().finish();
			return;
		}
		getActivity().setResult(Activity.RESULT_OK, inRet);
		getActivity().finish();
	}

	@Override
	public void onItemClick(AdapterView<?> parent, View view, int position,
			long id) {
		SbbsMeArticleObject item = list.get(position);
		if (item.objType == 0) {
			try {
				Intent inUrl = new Intent(Intent.ACTION_VIEW);
				inUrl.setData(Uri.parse(item.url));
				startActivity(inUrl);
			} catch (Exception e) {

			}
		} else if (item.objType == 1) {
			startActivity(new Intent(getActivity(), BigPictureActivity.class)
					.putExtra("image", item.url));
		}

	}

}
