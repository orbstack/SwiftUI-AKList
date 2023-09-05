/*
 * Copyright (c) 2023 Orbital Labs, LLC <danny@orbstack.dev>
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

import AppKit
import SwiftUI
import Combine

private let maxReuseSlots = 2

// Hierarchical list using AppKit's NSOutlineView and NSTreeController.
// Can also be used for non-hierarchical ("flat") lists.
//
// TO MIGRATE FROM SwiftUI List:
//   - List -> AKList
//   - Section -> [AKSection<Item>]
//   - .onRawDoubleClick -> .akListOnDoubleClick
//   - .contextMenu -> .akListContextMenu
//   - increase .vertical padding (4->8) to match SwiftUI List
//     * AKList doesn't add implicit padding
//   - add .environmentObjects to the item view
//     * needs to be reinjected across NSHostingView boundary
//   - (optional) set rowHeight: for performance
//
// benefits:
//   - fix black bar w/o covering up rect
//     -> fixes scrollbar
//   - slightly faster
//   - double click to expand
//   - no random holes / buggy behavior
//   - row separator lines, but only when selected
//   - should no longer crash
//   - scroll position moves to follow selection
//   - native double click implementation for reliability
//   - sections for hierarchical lists
//   - hides empty sections
struct AKList<Item: AKListItem, ItemView: View>: View {
    @StateObject private var envModel = AKListModel()

    private let sections: [AKSection<Item>]
    @Binding private var selection: Set<Item.ID>
    private let rowHeight: CGFloat?
    private let makeRowView: (Item) -> ItemView
    private var singleSelection = false
    private var flat = false

    // hierarchical OR flat, with sections, multiple selection
    init(_ sections: [AKSection<Item>],
         selection: Binding<Set<Item.ID>>,
         rowHeight: CGFloat? = nil,
         flat: Bool = true,
         @ViewBuilder makeRowView: @escaping (Item) -> ItemView) {
        self.sections = sections
        self._selection = selection
        self.rowHeight = rowHeight
        self.makeRowView = makeRowView
        self.flat = flat
    }

    var body: some View {
        AKTreeListImpl(envModel: envModel,
                sections: sections,
                rowHeight: rowHeight,
                singleSelection: singleSelection,
                isFlat: flat,
                makeRowView: makeRowView)
                // fix toolbar color and blur (fullSizeContentView)
        .ignoresSafeArea()
        .onReceive(envModel.$selection) { selection in
            self.selection = selection as! Set<Item.ID>
        }
    }
}

// structs can't have convenience init, so use an extension
extension AKList {
    // hierarchical OR flat, with sections, single selection
    init(_ sections: [AKSection<Item>],
         selection singleBinding: Binding<Item.ID?>,
         rowHeight: CGFloat? = nil,
         flat: Bool = true,
         @ViewBuilder makeRowView: @escaping (Item) -> ItemView) {
        let selBinding = Binding<Set<Item.ID>>(
                get: {
                    if let id = singleBinding.wrappedValue {
                        return [id]
                    } else {
                        return []
                    }
                },
                set: {
                    singleBinding.wrappedValue = $0.first
                })
        self.init(sections,
                selection: selBinding,
                rowHeight: rowHeight,
                flat: flat,
                makeRowView: makeRowView)
        self.singleSelection = true
    }

    // hierarchical OR flat, no sections, multiple selection
    init(_ items: [Item],
         selection: Binding<Set<Item.ID>>,
         rowHeight: CGFloat? = nil,
         flat: Bool = true,
         @ViewBuilder makeRowView: @escaping (Item) -> ItemView) {
        self.init(AKSection.single(items),
                selection: selection,
                rowHeight: rowHeight,
                flat: flat,
                makeRowView: makeRowView)
    }

    // hierarchical OR flat, no sections, single selection
    init(_ items: [Item],
         selection singleBinding: Binding<Item.ID?>,
         rowHeight: CGFloat? = nil,
         flat: Bool = true,
         @ViewBuilder makeRowView: @escaping (Item) -> ItemView) {
        self.init(AKSection.single(items),
                selection: singleBinding,
                rowHeight: rowHeight,
                flat: flat,
                makeRowView: makeRowView)
        self.singleSelection = true
    }
}

private class AKOutlineView: NSOutlineView {
    // workaround for off-center disclosure arrow: https://stackoverflow.com/a/74894605
    override func frameOfOutlineCell(atRow row: Int) -> NSRect {
        super.frameOfOutlineCell(atRow: row)
    }

    // we get here if the right click wasn't handled by SwiftUI, usually b/c out of bounds
    // e.g. clicked on arrow or margin
    // never use the fake menu, but try to forward it
    override func menu(for event: NSEvent) -> NSMenu? {
        // calling super.menu makes highlight ring appear, so do this ourselves
        // otherwise right-clicking section header triggers ring
        let targetRow = row(at: convert(event.locationInWindow, from: nil))

        // find the clicked view
        if targetRow != -1,
           let view = self.view(atColumn: 0, row: targetRow, makeIfNecessary: false) {
            // make a fake event for its center
            let center = CGPointMake(NSMidX(view.frame), NSMidY(view.frame))
            // ... relative to the window
            let centerInWindow = view.convert(center, to: nil)

            if let fakeEvent = NSEvent.mouseEvent(
                with: event.type,
                location: centerInWindow,
                modifierFlags: event.modifierFlags,
                timestamp: event.timestamp,
                windowNumber: event.windowNumber,
                context: nil, // deprecated
                eventNumber: event.eventNumber,
                clickCount: event.clickCount,
                pressure: event.pressure
            ) {
                return view.menu(for: fakeEvent)
            }
        }

        // failed to forward
        return nil
    }

    // for AKHostingView to trigger highlight
    func injectMenu(for event: NSEvent) {
        super.menu(for: event)
    }
}

// forward menu request/open/close events to NSOutlineView so it triggers highlight ring,
// but *actually* use the menu from SwiftUI
private class AKHostingView<V: View>: NSHostingView<V> {
    weak var outlineParent: AKOutlineView?
    var releaser: (() -> Void)?

    override func menu(for event: NSEvent) -> NSMenu? {
        // trigger NSOutlineView's highlight
        outlineParent?.injectMenu(for: event)
        return super.menu(for: event)
    }

    // forward menu events
    override func willOpenMenu(_ menu: NSMenu, with event: NSEvent) {
        super.willOpenMenu(menu, with: event)
        outlineParent?.willOpenMenu(menu, with: event)
    }

    override func didCloseMenu(_ menu: NSMenu, with event: NSEvent?) {
        super.didCloseMenu(menu, with: event)
        outlineParent?.didCloseMenu(menu, with: event)
    }

    override func mouseDown(with event: NSEvent) {
        super.mouseDown(with: event)
    }

    override func viewDidMoveToSuperview() {
        super.viewDidMoveToSuperview()
        if superview == nil {
            releaser?()
        }
    }
}

class AKListModel: ObservableObject {
    let doubleClicks = PassthroughSubject<AnyHashable, Never>()
    @Published var selection: Set<AnyHashable> = []
}

private class AKListItemModel: ObservableObject {
    // TODO silence AppCode inspection
    @Published var item: (any AKListItem)?
    @Published var itemId: AnyHashable

    init(itemId: AnyHashable) {
        self.itemId = itemId
    }
}

private class CachedViewHolder<V: View> {
    var view: AKHostingView<V>
    var model: AKListItemModel

    init(view: AKHostingView<V>, model: AKListItemModel) {
        self.view = view
        self.model = model
    }
}

typealias AKListItemBase = Identifiable & Equatable

protocol AKListItem: AKListItemBase {
    var listChildren: [any AKListItem]? { get }
}

extension AKListItem {
    var listChildren: [any AKListItem]? {
        nil
    }
}

struct AKSection<Element: AKListItem>: AKListItemBase {
    // nil = no header
    let title: String?
    let items: [Element]

    var id: String? {
        title
    }

    init(_ title: String?, _ items: [Element]) {
        self.title = title
        self.items = items
    }

    static func single(_ items: [Element]) -> [AKSection<Element>] {
        [AKSection(nil, items)]
    }
}

@objc protocol AKNode {}

private class AKItemNode: NSObject, AKNode {
    // don't try to be smart with these properties. NSTreeController requires KVO to work
    @objc dynamic var children: [AKItemNode]?
    @objc dynamic var isLeaf = true
    @objc dynamic var count = 0

    var value: any AKListItem

    init(value: any AKListItem) {
        self.value = value
    }
}

private class AKSectionNode: NSObject, AKNode {
    @objc dynamic var children: [AKItemNode]?
    @objc dynamic var isLeaf = true
    @objc dynamic var count = 0

    var value: String

    init(value: String, children: [AKItemNode]?) {
        self.value = value
        self.children = children
    }
}

private struct HostedItemView<Item: AKListItem, ItemView: View>: View {
    @ObservedObject var envModel: AKListModel
    @ObservedObject var itemModel: AKListItemModel

    @ViewBuilder let makeRowView: (Item) -> ItemView

    var body: some View {
        if let item = itemModel.item {
            makeRowView(item as! Item)
            .environmentObject(envModel)
            .environmentObject(itemModel)
        } else {
            EmptyView()
        }
    }
}

private struct AKTreeListImpl<Item: AKListItem, ItemView: View>: NSViewRepresentable {
    typealias Section = AKSection<Item>
    typealias CachedView = CachedViewHolder<HostedItemView<Item, ItemView>>

    @ObservedObject var envModel: AKListModel

    let sections: [Section]
    let rowHeight: CGFloat?
    let singleSelection: Bool
    let isFlat: Bool
    let makeRowView: (Item) -> ItemView

    final class Coordinator: NSObject, NSOutlineViewDelegate {
        var parent: AKTreeListImpl

        @objc fileprivate dynamic var content: [AKNode] = []
        var lastSections: [Section]?

        private var observation: NSKeyValueObservation?
        var treeController: NSTreeController? {
            didSet {
                // KVO-observing selectedObjects is better than outlineViewSelectionDidChange
                // because it changes to empty when items are deleted
                observation = treeController?.observe(\.selectedObjects) { [weak self] _, _ in
                    guard let self, let treeController else { return }
                    let selectedIds = treeController.selectedObjects
                        .compactMap { ($0 as? AKItemNode)?.value.id as? Item.ID }
                    // Publishing changes from within view updates is not allowed, this will cause undefined behavior.
                    let newSelection = Set(selectedIds) as Set<AnyHashable>
                    if self.parent.envModel.selection != newSelection {
                        DispatchQueue.main.async {
                            self.parent.envModel.selection = newSelection
                        }
                    }
                }
            }
        }

        // preserve objc object identity to avoid losing state
        // overriding isEqual would probably work but this is also good for perf
        private var objCache = [Item.ID: AKItemNode]()
        // array is fastest since we just iterate and clear this
        private var objAccessTracker = [Item.ID]()

        // preserve view identity to avoid losing state (e.g. popovers)
        private var viewCache = [Item.ID: CachedView]()
        // custom reuse queue. hard to use nibs, and we need the identity-preserving cache logic too
        private var reuseQueue = [CachedView]()

        init(_ parent: AKTreeListImpl) {
            self.parent = parent
            reuseQueue.reserveCapacity(maxReuseSlots)
        }

        private func getOrCreateItemView(outlineView: NSOutlineView, itemId: Item.ID) -> CachedView {
            // 1. cached for ID, to preserve identity
            if let cached = viewCache[itemId] {
                return cached
            }

            // 2. look for reusable one
            if let cached = reuseQueue.popLast() {
                // a reused view should be added back to the cache once it's been rebound
                viewCache[itemId] = cached
                return cached
            }

            // 3. make a new one
            let itemModel = AKListItemModel(itemId: itemId as AnyHashable)
            // doing .environmentObject in the SwiftUI view lets us avoid AnyView here
            let hostedView = HostedItemView(envModel: parent.envModel,
                    itemModel: itemModel,
                    makeRowView: parent.makeRowView)
            let nsView = AKHostingView(rootView: hostedView)
            nsView.outlineParent = (outlineView as! AKOutlineView)

            let cached = CachedView(view: nsView, model: itemModel)
            viewCache[itemId] = cached

            // set releaser
            nsView.releaser = { [weak self, weak cached] in
                guard let self, let cached else { return }
                // remove from active cache
                self.viewCache.removeValue(forKey: cached.model.itemId as! Item.ID)
                // add to reuse queue if space is available
                if self.reuseQueue.count < maxReuseSlots {
                    self.reuseQueue.append(cached)
                }
                // remove item
                cached.model.item = nil
            }

            return cached
        }

        // make views
        func outlineView(_ outlineView: NSOutlineView, viewFor tableColumn: NSTableColumn?, item: Any) -> NSView? {
            let nsNode = item as! NSTreeNode

            if let node = nsNode.representedObject as? AKItemNode {
                let cached = getOrCreateItemView(outlineView: outlineView, itemId: node.value.id as! Item.ID)

                // update value if needed
                // updateNSView does async so it's fine to update right here
                if (cached.model.item as? Item) != (node.value as? Item) {
                    cached.model.item = node.value
                }
                if (cached.model.itemId as? Item.ID) != (node.value.id as? Item.ID) {
                    cached.model.itemId = node.value.id as! Item.ID
                }

                return cached.view
            } else if let node = nsNode.representedObject as? AKSectionNode {
                // pixel-perfect match of SwiftUI default section header
                let cellView = NSTableCellView()
                let field = NSTextField(labelWithString: node.value)
                field.font = NSFont.systemFont(ofSize: NSFont.smallSystemFontSize, weight: .semibold)
                field.textColor = .secondaryLabelColor
                field.isEditable = false
                cellView.addSubview(field)

                // center vertically, align to left
                field.translatesAutoresizingMaskIntoConstraints = false
                NSLayoutConstraint.activate([
                    field.leadingAnchor.constraint(equalTo: cellView.leadingAnchor),
                    field.centerYAnchor.constraint(equalTo: cellView.centerYAnchor),
                ])

                return cellView
            } else {
                return nil
            }
        }

        // at first glance this isn't needed because section nodes don't render selections,
        // but it still gets selected internally and breaks the rounding of adjacent rows
        func outlineView(_ outlineView: NSOutlineView, shouldSelectItem item: Any) -> Bool {
            let nsNode = item as! NSTreeNode
            if nsNode.representedObject is AKItemNode {
                return true
            } else {
                return false
            }
        }

        func outlineView(_ outlineView: NSOutlineView, heightOfRowByItem item: Any) -> CGFloat {
            let nsNode = item as! NSTreeNode
            if nsNode.representedObject is AKItemNode {
                return parent.rowHeight ?? outlineView.rowHeight
            } else if nsNode.representedObject is AKSectionNode {
                // match SwiftUI section
                return 28
            } else {
                return 0
            }
        }

        @objc func onDoubleClick(_ sender: Any) {
            // expand or collapse row
            let outlineView = sender as! NSOutlineView
            let row = outlineView.clickedRow
            guard row != -1 else {
                return
            }

            let item = outlineView.item(atRow: row)
            if outlineView.isItemExpanded(item) {
                outlineView.animator().collapseItem(item)
            } else {
                outlineView.animator().expandItem(item)
            }

            // emit double click event via notification center
            let nsNode = item as! NSTreeNode
            if let node = nsNode.representedObject as? AKItemNode {
                parent.envModel.doubleClicks.send(node.value.id as! AnyHashable)
            }
        }

        func mapNode(item: Item) -> AKItemNode {
            var node: AKItemNode
            if let cachedNode = objCache[item.id] {
                node = cachedNode
            } else {
                node = AKItemNode(value: item)
                objCache[item.id] = node
            }
            objAccessTracker.append(item.id)

            var nodeChildren = item.listChildren?.map { mapNode(item: $0 as! Item) }
            // map empty to nil
            if nodeChildren?.isEmpty ?? false {
                nodeChildren = nil
            }

            // do we need to update this node? if not, avoid triggering NSTreeController's KVO
            // isLeaf and count are derived from children, so no need to check
            if (node.value as! Item) != item || nodeChildren != node.children {
                if let nodeChildren {
                    node.children = nodeChildren
                    node.isLeaf = false
                    node.count = nodeChildren.count
                } else {
                    node.children = nil
                    node.isLeaf = true
                    node.count = 0
                }
                node.value = item
            }
            return node
        }

        func mapAllNodes(sections: [Section]) -> [AKNode] {
            // record accessed nodes
            let newNodes = sections.flatMap {
                // don't show empty sections
                if $0.items.isEmpty {
                    return [AKNode]()
                }

                // more efficient than map and concat
                var sectionNodes = [AKNode]()
                sectionNodes.reserveCapacity($0.items.count + 1)
                if let title = $0.title {
                    // TODO: if we use children, then groups are collapsible
                    sectionNodes.append(AKSectionNode(value: title, children: nil))
                }
                for item in $0.items {
                    sectionNodes.append(mapNode(item: item))
                }
                return sectionNodes
            }

            // remove unused nodes
            let unusedNodes = objCache.filter { !objAccessTracker.contains($0.key) }
            for (id, _) in unusedNodes {
                objCache.removeValue(forKey: id)
            }

            // clear access tracker
            objAccessTracker.removeAll()
            return newNodes
        }

        func outlineView(_ outlineView: NSOutlineView, isGroupItem item: Any) -> Bool {
            let nsNode = item as! NSTreeNode
            return nsNode.representedObject is AKSectionNode
        }
    }

    func makeNSView(context: Context) -> NSScrollView {
        let coordinator = context.coordinator
        coordinator.parent = self

        let treeController = NSTreeController()
        treeController.bind(.contentArray, to: coordinator, withKeyPath: "content")
        treeController.objectClass = AKItemNode.self
        treeController.childrenKeyPath = "children"
        treeController.countKeyPath = "count"
        treeController.leafKeyPath = "isLeaf"
        treeController.preservesSelection = true
        treeController.avoidsEmptySelection = false
        treeController.selectsInsertedObjects = false
        treeController.alwaysUsesMultipleValuesMarker = true // perf
        coordinator.treeController = treeController

        let outlineView = AKOutlineView()
        outlineView.delegate = coordinator
        outlineView.bind(.content, to: treeController, withKeyPath: "arrangedObjects")
        outlineView.bind(.selectionIndexPaths, to: treeController, withKeyPath: "selectionIndexPaths")
        // fix width changing when expanding/collapsing
        outlineView.autoresizesOutlineColumn = false
        outlineView.allowsMultipleSelection = !singleSelection
        outlineView.allowsEmptySelection = true
        if let rowHeight {
            outlineView.rowHeight = rowHeight
        } else {
            outlineView.usesAutomaticRowHeights = true
        }
        if isFlat {
            // remove padding at left
            outlineView.indentationPerLevel = 0
        }
        // dummy menu to trigger highlight
        outlineView.menu = NSMenu()

        // hide header
        outlineView.headerView = nil

        // use outlineView's double click. more reliable than Swift onDoubleClick
        outlineView.target = coordinator
        outlineView.doubleAction = #selector(Coordinator.onDoubleClick)

        // add one column
        let column = NSTableColumn(identifier: NSUserInterfaceItemIdentifier("column"))
        column.isEditable = false
        outlineView.addTableColumn(column)

        let scrollView = NSScrollView()
        scrollView.documentView = outlineView
        scrollView.hasVerticalScroller = true
        return scrollView
    }

    func updateNSView(_ nsView: NSScrollView, context: Context) {
        let coordinator = context.coordinator
        coordinator.parent = self
        guard sections != coordinator.lastSections else {
            return
        }

        // convert to nodes
        // DispatchQueue.main.async causes initial flicker,
        // but later we need it to avoid AttributeGraph cycles when clicking popovers during updates
        // because updating .content updates SwiftUI hosting views, but updateNSView is called inside a SwiftUI view update
        // this makes the updating non-atomic but it's fine
        if coordinator.lastSections == nil {
            let nodes = coordinator.mapAllNodes(sections: sections)
            // update tree controller and reload view (via KVO)
            coordinator.content = nodes
        } else {
            DispatchQueue.main.async {
                let nodes = coordinator.mapAllNodes(sections: sections)
                coordinator.content = nodes
            }
        }
        coordinator.lastSections = sections
    }

    static func dismantleNSView(_ nsView: NSViewType, coordinator: Coordinator) {
        // break KVO reference cycle
        coordinator.treeController = nil
    }

    func makeCoordinator() -> Coordinator {
        Coordinator(self)
    }
}

private struct DoubleClickViewModifier: ViewModifier {
    @EnvironmentObject private var listModel: AKListModel
    @EnvironmentObject private var itemModel: AKListItemModel

    let action: () -> Void

    func body(content: Content) -> some View {
        content
            .onReceive(listModel.doubleClicks) { id in
                if id == itemModel.itemId {
                    action()
                }
            }
    }
}

private struct BoundingBoxOverlayView: NSViewRepresentable {
    func makeNSView(context: Context) -> NSView {
        NSView(frame: .zero)
    }

    func updateNSView(_ nsView: NSView, context: Context) {
    }
}

extension View {
    // SwiftUI rejects menu(forEvent:) unless it thinks it owns the view at which
    // the click occurred. add a big NSView overlay to fix it
    func akListContextMenu<MenuItems: View>(@ViewBuilder menuItems: () -> MenuItems) -> some View {
        self
            .overlay { BoundingBoxOverlayView() }
            .contextMenu(menuItems: menuItems)
    }

    func akListOnDoubleClick(perform action: @escaping () -> Void) -> some View {
        self.modifier(DoubleClickViewModifier(action: action))
    }
}
