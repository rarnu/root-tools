package com.rarnu.devlib.component;

import android.content.Context;
import android.graphics.Canvas;
import android.graphics.drawable.Drawable;
import android.util.AttributeSet;
import android.util.SparseArray;
import android.view.View;
import android.view.ViewGroup;
import android.view.animation.Animation;
import android.view.animation.Animation.AnimationListener;
import android.view.animation.Transformation;
import android.widget.AbsListView;

import java.util.ArrayList;
import java.util.List;

public class ExpandableListView extends android.widget.ExpandableListView {

    private int animationDuration = 500;

    private BaseExpandableListAdapter adapter;

    public ExpandableListView(Context context) {
        super(context);
    }

    public ExpandableListView(Context context, AttributeSet attrs) {
        super(context, attrs);
    }

    public ExpandableListView(Context context, AttributeSet attrs, int defStyle) {
        super(context, attrs, defStyle);
    }

    public void setAdapter(BaseExpandableListAdapter adapter) {
        super.setAdapter(adapter);
        this.adapter = adapter;
        this.adapter.setParent(this);
    }

    public boolean expandGroupWithAnimation(int groupPos) {
        int groupFlatPos = getFlatListPosition(getPackedPositionForGroup(groupPos));
        if (groupFlatPos != -1) {
            int childIndex = groupFlatPos - getFirstVisiblePosition();
            if (childIndex < getChildCount()) {

                View v = getChildAt(childIndex);
                if (v.getBottom() >= getBottom()) {
                    adapter.notifyGroupExpanded(groupPos);
                    return expandGroup(groupPos);
                }
            }
        }

        adapter.startExpandAnimation(groupPos, 0);
        return expandGroup(groupPos);
    }

    public boolean collapseGroupWithAnimation(int groupPos) {
        int groupFlatPos = getFlatListPosition(getPackedPositionForGroup(groupPos));
        if (groupFlatPos != -1) {
            int childIndex = groupFlatPos - getFirstVisiblePosition();
            if (childIndex >= 0 && childIndex < getChildCount()) {
                View v = getChildAt(childIndex);
                if (v.getBottom() >= getBottom()) {
                    return collapseGroup(groupPos);
                }
            } else {
                return collapseGroup(groupPos);
            }
        }

        long packedPos = getExpandableListPosition(getFirstVisiblePosition());
        int firstChildPos = getPackedPositionChild(packedPos);
        int firstGroupPos = getPackedPositionGroup(packedPos);

        firstChildPos = firstChildPos == -1 || firstGroupPos != groupPos ? 0 : firstChildPos;
        adapter.startCollapseAnimation(groupPos, firstChildPos);
        adapter.notifyDataSetChanged();
        return isGroupExpanded(groupPos);
    }

    public int getAnimationDuration() {
        return animationDuration;
    }

    public void setAnimationDuration(int d) {
        animationDuration = d;
    }

    private static class GroupInfo {
        boolean animating = false;
        boolean expanding = false;
        int firstChildPosition;

        int dummyHeight = -1;
    }

    public static abstract class BaseExpandableListAdapter extends android.widget.BaseExpandableListAdapter {
        private SparseArray<GroupInfo> groupInfo = new SparseArray<GroupInfo>();
        private ExpandableListView parent;

        private static final int STATE_IDLE = 0;
        private static final int STATE_EXPANDING = 1;
        private static final int STATE_COLLAPSING = 2;

        private void setParent(ExpandableListView parent) {
            this.parent = parent;
        }

        public int getRealChildType(int groupPosition, int childPosition) {
            return 0;
        }

        public int getRealChildTypeCount() {
            return 1;
        }

        public abstract View getRealChildView(int groupPosition, int childPosition, boolean isLastChild, View convertView, ViewGroup parent);

        public abstract int getRealChildrenCount(int groupPosition);

        private GroupInfo getGroupInfo(int groupPosition) {
            GroupInfo info = groupInfo.get(groupPosition);
            if (info == null) {
                info = new GroupInfo();
                groupInfo.put(groupPosition, info);
            }
            return info;
        }

        public void notifyGroupExpanded(int groupPosition) {
            GroupInfo info = getGroupInfo(groupPosition);
            info.dummyHeight = -1;
        }

        private void startExpandAnimation(int groupPosition, int firstChildPosition) {
            GroupInfo info = getGroupInfo(groupPosition);
            info.animating = true;
            info.firstChildPosition = firstChildPosition;
            info.expanding = true;
        }

        private void startCollapseAnimation(int groupPosition, int firstChildPosition) {
            GroupInfo info = getGroupInfo(groupPosition);
            info.animating = true;
            info.firstChildPosition = firstChildPosition;
            info.expanding = false;
        }

        private void stopAnimation(int groupPosition) {
            GroupInfo info = getGroupInfo(groupPosition);
            info.animating = false;
        }

        @Override
        public final int getChildType(int groupPosition, int childPosition) {
            GroupInfo info = getGroupInfo(groupPosition);
            if (info.animating) {
                return 0;
            } else {
                return getRealChildType(groupPosition, childPosition) + 1;
            }
        }

        @Override
        public final int getChildTypeCount() {
            return getRealChildTypeCount() + 1;
        }

        protected ViewGroup.LayoutParams generateDefaultLayoutParams() {
            return new AbsListView.LayoutParams(ViewGroup.LayoutParams.MATCH_PARENT,
                    ViewGroup.LayoutParams.WRAP_CONTENT, 0);
        }

        @Override
        public final View getChildView(final int groupPosition, int childPosition, boolean isLastChild, View convertView, final ViewGroup parent) {
            final GroupInfo info = getGroupInfo(groupPosition);

            if (info.animating) {
                if (convertView instanceof DummyView == false) {
                    convertView = new DummyView(parent.getContext());
                    convertView.setLayoutParams(new AbsListView.LayoutParams(LayoutParams.MATCH_PARENT, 0));
                }

                if (childPosition < info.firstChildPosition) {
                    convertView.getLayoutParams().height = 0;
                    return convertView;
                }

                final ExpandableListView listView = (ExpandableListView) parent;
                final DummyView dummyView = (DummyView) convertView;
                dummyView.clearViews();
                dummyView.setDivider(listView.getDivider(), parent.getMeasuredWidth(), listView.getDividerHeight());
                final int measureSpecW = MeasureSpec.makeMeasureSpec(parent.getWidth(), MeasureSpec.EXACTLY);
                final int measureSpecH = MeasureSpec.makeMeasureSpec(0, MeasureSpec.UNSPECIFIED);

                int totalHeight = 0;
                int clipHeight = parent.getHeight();

                final int len = getRealChildrenCount(groupPosition);
                for (int i = info.firstChildPosition; i < len; i++) {
                    View childView = getRealChildView(groupPosition, i, (i == len - 1), null, parent);

                    LayoutParams p = (LayoutParams) childView.getLayoutParams();
                    if (p == null) {
                        p = (AbsListView.LayoutParams) generateDefaultLayoutParams();
                        childView.setLayoutParams(p);
                    }

                    int lpHeight = p.height;

                    int childHeightSpec;
                    if (lpHeight > 0) {
                        childHeightSpec = MeasureSpec.makeMeasureSpec(lpHeight, MeasureSpec.EXACTLY);
                    } else {
                        childHeightSpec = measureSpecH;
                    }

                    childView.measure(measureSpecW, childHeightSpec);
                    totalHeight += childView.getMeasuredHeight();

                    if (totalHeight < clipHeight) {
                        dummyView.addFakeView(childView);
                    } else {
                        dummyView.addFakeView(childView);
                        int averageHeight = totalHeight / (i + 1);
                        totalHeight += (len - i - 1) * averageHeight;
                        break;
                    }
                }

                Object o;
                int state = (o = dummyView.getTag()) == null ? STATE_IDLE : (Integer) o;

                if (info.expanding && state != STATE_EXPANDING) {
                    ExpandAnimation ani = new ExpandAnimation(dummyView, 0, totalHeight, info);
                    ani.setDuration(this.parent.getAnimationDuration());
                    ani.setAnimationListener(new AnimationListener() {

                        @Override
                        public void onAnimationEnd(Animation animation) {
                            stopAnimation(groupPosition);
                            notifyDataSetChanged();
                            dummyView.setTag(STATE_IDLE);
                        }

                        @Override
                        public void onAnimationRepeat(Animation animation) {
                        }

                        @Override
                        public void onAnimationStart(Animation animation) {
                        }

                    });
                    dummyView.startAnimation(ani);
                    dummyView.setTag(STATE_EXPANDING);
                } else if (!info.expanding && state != STATE_COLLAPSING) {
                    if (info.dummyHeight == -1) {
                        info.dummyHeight = totalHeight;
                    }

                    ExpandAnimation ani = new ExpandAnimation(dummyView, info.dummyHeight, 0, info);
                    ani.setDuration(this.parent.getAnimationDuration());
                    ani.setAnimationListener(new AnimationListener() {

                        @Override
                        public void onAnimationEnd(Animation animation) {
                            stopAnimation(groupPosition);
                            listView.collapseGroup(groupPosition);
                            notifyDataSetChanged();
                            info.dummyHeight = -1;
                            dummyView.setTag(STATE_IDLE);
                        }

                        @Override
                        public void onAnimationRepeat(Animation animation) {
                        }

                        @Override
                        public void onAnimationStart(Animation animation) {
                        }

                    });
                    dummyView.startAnimation(ani);
                    dummyView.setTag(STATE_COLLAPSING);
                }

                return convertView;
            } else {
                return getRealChildView(groupPosition, childPosition, isLastChild, convertView, parent);
            }
        }

        @Override
        public final int getChildrenCount(int groupPosition) {
            GroupInfo info = getGroupInfo(groupPosition);
            if (info.animating) {
                return info.firstChildPosition + 1;
            } else {
                return getRealChildrenCount(groupPosition);
            }
        }

    }

    private static class DummyView extends View {
        private List<View> views = new ArrayList<View>();
        private Drawable divider;
        private int dividerWidth;
        private int dividerHeight;

        public DummyView(Context context) {
            super(context);
        }

        public void setDivider(Drawable divider, int dividerWidth, int dividerHeight) {
            if (divider != null) {
                this.divider = divider;
                this.dividerWidth = dividerWidth;
                this.dividerHeight = dividerHeight;

                divider.setBounds(0, 0, dividerWidth, dividerHeight);
            }
        }

        public void addFakeView(View childView) {
            childView.layout(0, 0, getWidth(), childView.getMeasuredHeight());
            views.add(childView);
        }

        @Override
        protected void onLayout(boolean changed, int left, int top, int right, int bottom) {
            super.onLayout(changed, left, top, right, bottom);
            final int len = views.size();
            for (int i = 0; i < len; i++) {
                View v = views.get(i);
                v.layout(left, top, left + v.getMeasuredWidth(), top + v.getMeasuredHeight());
            }
        }

        public void clearViews() {
            views.clear();
        }

        @Override
        public void dispatchDraw(Canvas canvas) {
            canvas.save();
            if (divider != null) {
                divider.setBounds(0, 0, dividerWidth, dividerHeight);
            }

            final int len = views.size();
            for (int i = 0; i < len; i++) {
                View v = views.get(i);

                canvas.save();
                canvas.clipRect(0, 0, getWidth(), v.getMeasuredHeight());
                v.draw(canvas);
                canvas.restore();

                if (divider != null) {
                    divider.draw(canvas);
                    canvas.translate(0, dividerHeight);
                }

                canvas.translate(0, v.getMeasuredHeight());
            }

            canvas.restore();
        }
    }

    private static class ExpandAnimation extends Animation {
        private int baseHeight;
        private int delta;
        private View view;
        private GroupInfo groupInfo;

        private ExpandAnimation(View v, int startHeight, int endHeight, GroupInfo info) {
            baseHeight = startHeight;
            delta = endHeight - startHeight;
            view = v;
            groupInfo = info;

            view.getLayoutParams().height = startHeight;
            view.requestLayout();
        }

        @Override
        protected void applyTransformation(float interpolatedTime, Transformation t) {
            super.applyTransformation(interpolatedTime, t);
            if (interpolatedTime < 1.0f) {
                int val = baseHeight + (int) (delta * interpolatedTime);
                view.getLayoutParams().height = val;
                groupInfo.dummyHeight = val;
                view.requestLayout();
            } else {
                int val = baseHeight + delta;
                view.getLayoutParams().height = val;
                groupInfo.dummyHeight = val;
                view.requestLayout();
            }
        }
    }
}
