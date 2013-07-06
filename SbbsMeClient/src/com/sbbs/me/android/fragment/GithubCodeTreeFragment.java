package com.sbbs.me.android.fragment;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.egit.github.core.TreeEntry;

import android.annotation.SuppressLint;
import android.content.Intent;
import android.content.Loader;
import android.content.Loader.OnLoadCompleteListener;
import android.graphics.drawable.ColorDrawable;
import android.os.Bundle;
import android.util.Log;
import android.view.Menu;
import android.view.View;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemClickListener;
import android.widget.TextView;

import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.devlib.component.PullDownListView;
import com.rarnu.devlib.component.intf.OnPullDownListener;
import com.rarnu.utils.ResourceUtils;
import com.rarnu.utils.UIUtils;
import com.sbbs.me.android.CodeTreeActivity;
import com.sbbs.me.android.CodeViewActivity;
import com.sbbs.me.android.Global;
import com.sbbs.me.android.R;
import com.sbbs.me.android.adapter.SbbsMeTreeEntryAdapter;
import com.sbbs.me.android.loader.SbbsCodeTreeLoader;
import static org.eclipse.egit.github.core.TreeEntry.TYPE_BLOB;
import static org.eclipse.egit.github.core.TreeEntry.TYPE_TREE;

@SuppressLint("ValidFragment")
public class GithubCodeTreeFragment extends BaseFragment 
		implements OnLoadCompleteListener<List<TreeEntry>>,
		OnPullDownListener,OnItemClickListener {
	
	PullDownListView treePullDown;
	SbbsCodeTreeLoader loader;
	SbbsMeTreeEntryAdapter adapter;
	TextView treeLoading;
	List<TreeEntry> listTreeEntry;
	byte repoType = 0;
	String sha;
		
	public GithubCodeTreeFragment(byte repoType, String sha) {
		super();
		this.repoType = repoType;
		this.sha = sha;
		tagText = ResourceUtils
				.getString(repoType == 0 ? R.tag.tag_ongithub_fragment_sbbs
						: R.tag.tag_ongithub_fragment_android);
		tabTitle = ResourceUtils.getString(repoType == 0 ? R.string.project_sbbs_me
				: R.string.project_android);
	}

	@Override
	public int getBarTitle() {
		return R.string.lm_ongithub;
	}

	@Override
	public int getBarTitleWithPath() {
		return R.string.lm_ongithub;
	}

	@Override
	public String getCustomTitle() {
		return null;
	}

	@Override
	public void initComponents() {
		treePullDown = (PullDownListView) innerView
				.findViewById(R.id.treePullDown);
		treeLoading = (TextView) innerView.findViewById(R.id.treeLoading);
		if (listTreeEntry == null) {
			listTreeEntry = new ArrayList<TreeEntry>();
		}
		adapter =  new SbbsMeTreeEntryAdapter(getActivity(), listTreeEntry);
		treePullDown.getListView().setAdapter(adapter);
		
		treePullDown.enableAutoFetchMore(true, 1);
		treePullDown.setOnPullDownListener(this);
		treePullDown.getListView().setDivider(new ColorDrawable(0xFFc5eaf8));
		treePullDown.getListView().setDividerHeight(UIUtils.dipToPx(1));
		
		loader = new SbbsCodeTreeLoader(getActivity(), repoType, sha);
	}

	@Override
	public void initEvents() {
		treePullDown.getListView().setOnItemClickListener(this);
		loader.registerListener(0, this);
	}

	@Override
	public void initLogic() {
		if (listTreeEntry.size() == 0) {
			treeLoading.setVisibility(View.VISIBLE);
			loader.startLoading();
		}
		treePullDown.notifyDidLoad();
	}

	@Override
	public int getFragmentLayoutResId() {
		return R.layout.fragment_codetree;
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
	public void onLoadComplete(Loader<List<TreeEntry>> loader,
			List<TreeEntry> data) {
		listTreeEntry.clear();
		if (data != null) {
			listTreeEntry.addAll(data);
		}
		adapter.setNewList(listTreeEntry);
		treeLoading.setVisibility(View.GONE);
		treePullDown.notifyDidRefresh();
	}

	@Override
	public void onItemClick(AdapterView<?> parent, View view, int position, 
			long id) {
		final TreeEntry item = (TreeEntry) treePullDown.getListView()
				.getItemAtPosition(position);
		Log.e("onItemClick", item.getSha());
		loader.setSha(item.getSha());
		loader.startLoading();
		
	}

	@Override
	public void onRefresh() {
		loader.startLoading();
	}

	@Override
	public void onMore() {
		
	}
}
