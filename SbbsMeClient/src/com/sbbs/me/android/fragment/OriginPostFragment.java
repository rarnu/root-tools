package com.sbbs.me.android.fragment;

import java.util.List;

import android.os.Bundle;
import android.view.Menu;
import android.widget.RelativeLayout;

import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.utils.ResourceUtils;
import com.sbbs.me.android.Global;
import com.sbbs.me.android.R;
import com.sbbs.me.android.api.SbbsMeAPI;
import com.sbbs.me.android.api.SbbsMeArticle;
import com.sbbs.me.android.api.SbbsMeBlock;
import com.sbbs.me.android.api.SbbsMeSideBlocks;
import com.sbbs.me.android.utils.CustomUtils;

public class OriginPostFragment extends BaseFragment {

	RelativeLayout layPost;
	SbbsMeBlock block;
	String blockId;
	SbbsMeArticle article;
	List<SbbsMeBlock> listPost;

	public OriginPostFragment() {
		super();
		tagText = ResourceUtils.getString(R.string.tag_origin_post_fragment);
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
		return null;
	}

	@Override
	public void initComponents() {
		layPost = (RelativeLayout) innerView.findViewById(R.id.layPost);
	}

	@Override
	public void initEvents() {

	}

	@Override
	public void initLogic() {

	}

	@Override
	public int getFragmentLayoutResId() {
		return R.layout.fragment_origin_post;
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
		buildUI();
	}

	private void buildUI() {
		layPost.removeAllViews();
		listPost = article.left_blocks.get(blockId);

		if (listPost != null && listPost.size() != 0) {
			int viewId = 120000;
			SbbsMeSideBlocks sb = null;
			for (int i = 0; i < listPost.size(); i++) {
				sb = SbbsMeAPI.getSideBlocks(article, listPost.get(i).Id);
				CustomUtils.addBlock(getActivity(), listPost.get(i),
						sb.leftBlockCount, sb.rightBlockCount,
						article.users.get(listPost.get(i).AuthorId), layPost,
						viewId, 120000, true, null, null);
				viewId++;
			}
		}
	}

	@Override
	public Bundle getFragmentState() {
		return null;
	}

}
