package com.yugioh.android.adapter;

import android.content.Context;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import com.rarnu.devlib.adapter.BaseAdapter;
import com.rarnu.utils.DownloadUtils;
import com.yugioh.android.R;
import com.yugioh.android.classes.RecommandInfo;
import com.yugioh.android.define.NetworkDefine;
import com.yugioh.android.define.PathDefine;

import java.util.List;

public class RecommandAdapter extends BaseAdapter<RecommandInfo> {

    public RecommandAdapter(Context context, List<RecommandInfo> list) {
        super(context, list);
    }

    @Override
    public View getView(int position, View convertView, ViewGroup parent) {
        View v = convertView;
        if (v == null) {
            v = inflater.inflate(R.layout.item_recommand, parent, false);
        }
        RecommandHolder holder = (RecommandHolder) v.getTag();
        if (holder == null) {
            holder = new RecommandHolder();
            holder.ivRecommand = (ImageView) v.findViewById(R.id.ivRecommand);
            v.setTag(holder);
        }

        RecommandInfo item = list.get(position);
        if (item != null) {
            DownloadUtils.downloadFileT(context, holder.ivRecommand, NetworkDefine.RECOMMAND_IMAGE_URL + item.imagePath, PathDefine.RECOMMAND_PATH, item.imagePath, null);
        }
        return v;
    }

    @Override
    public String getValueText(RecommandInfo item) {
        return "";
    }

}
