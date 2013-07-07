package com.sbbs.me.android.fragment;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.egit.github.core.TreeEntry;

import android.content.Intent;
import android.content.Loader;
import android.content.Loader.OnLoadCompleteListener;
import android.os.Bundle;
import android.util.Log;
import android.view.Menu;
import android.view.View;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemClickListener;
import android.widget.ListView;
import android.widget.TextView;

import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.utils.ResourceUtils;
import com.rarnu.utils.UIUtils;
import com.sbbs.me.android.CodeViewActivity;
import com.sbbs.me.android.R;
import com.sbbs.me.android.adapter.SbbsMeTreeEntryAdapter;
import com.sbbs.me.android.loader.SbbsCodeTreeLoader;
import static org.eclipse.egit.github.core.TreeEntry.TYPE_BLOB;
import static org.eclipse.egit.github.core.TreeEntry.TYPE_TREE;

public class GithubCodeTreeFragment extends BaseFragment 
		implements OnLoadCompleteListener<List<TreeEntry>>,
		OnItemClickListener {
	
	ListView treeList;
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
		treeList = (ListView) innerView.findViewById(R.id.treeList);
		treeLoading = (TextView) innerView.findViewById(R.id.treeLoading);
		if (listTreeEntry == null) {
			listTreeEntry = new ArrayList<TreeEntry>();
		}
		adapter = new SbbsMeTreeEntryAdapter(getActivity(), listTreeEntry);
		treeList.setAdapter(adapter);
		treeList.setDivider(null);
		int devide = UIUtils.dipToPx(8);
		treeList.setDividerHeight(devide);
		treeList.setPadding(devide, devide, devide, devide);
		loader = new SbbsCodeTreeLoader(getActivity(), repoType, sha);
	}

	@Override
	public void initEvents() {
		treeList.setOnItemClickListener(this);
		loader.registerListener(0, this);
	}

	@Override
	public void initLogic() {
		treeLoading.setVisibility(View.VISIBLE);
		loader.startLoading();
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
		if ( data != null ) {
			listTreeEntry.addAll(data);
			adapter.setNewList(listTreeEntry);
		}
		treeLoading.setVisibility(View.GONE);
	}

	@Override
	public void onItemClick(AdapterView<?> parent, View view, int position, 
			long id) {
		final TreeEntry item = listTreeEntry.get(position);
		Log.e("onItemClick", item.getSha());
		Bundle bn = new Bundle();
		bn.putByte("repoType", repoType);
		bn.putString("sha", item.getSha());
		if ( item.getType().equals(TYPE_TREE) ) {
			loader.setSha(item.getSha());
			loader.startLoading();
		} else if ( item.getType().equals(TYPE_BLOB) ) {
			startActivity(new Intent(getActivity(), 
					CodeViewActivity.class).putExtras(bn));
		}
	}
}
