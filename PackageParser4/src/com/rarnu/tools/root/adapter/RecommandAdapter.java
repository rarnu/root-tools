package com.rarnu.tools.root.adapter;

import java.util.List;

import android.content.Context;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.TextView;

import com.rarnu.root.pp4.R;
import com.rarnu.tools.root.api.MobileApi;
import com.rarnu.tools.root.common.RecommandInfo;
import com.rarnu.tools.root.holder.RecommandAdapterHolder;
import com.rarnu.tools.root.utils.DirHelper;
import com.rarnu.tools.root.utils.DownloadUtils;

public class RecommandAdapter extends InnerAdapter<RecommandInfo> {

	public RecommandAdapter(Context context, List<RecommandInfo> list) {
		super(context, list);
	}

	@Override
	public View getView(int position, View convertView, ViewGroup parent) {
		View v = convertView;
		if (v == null) {
			v = inflater.inflate(R.layout.recommand_item, parent, false);
		}
		RecommandAdapterHolder holder = (RecommandAdapterHolder) v.getTag();
		if (holder == null) {
			holder = new RecommandAdapterHolder();
			holder.imgItemRecommand = (ImageView) v
					.findViewById(R.id.imgItemRecommand);
			holder.tvItemRecommand = (TextView) v
					.findViewById(R.id.tvItemRecommand);
			v.setTag(holder);
		}

		RecommandInfo item = list.get(position);
		if (item != null) {
			holder.tvItemRecommand.setText(item.name);
			DownloadUtils.downloadFileT(context, holder.imgItemRecommand,
					MobileApi.ICON_BASE_URL + item.iconUrl, DirHelper.TEMP_DIR,
					item.unixName, null);
		}

		return v;
	}

	@Override
	public String getValueText(RecommandInfo item) {
		return "";
	}

}
