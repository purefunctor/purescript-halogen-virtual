import { 
  elementScroll, 
  observeElementOffset, 
  observeElementRect, 
  Virtualizer 
} from "@tanstack/virtual-core";

export function mkVirtualizer(options) {
  return new Virtualizer({
    ...options, 
    observeElementOffset, 
    observeElementRect, 
    scrollToFn: elementScroll
  });
}

export function getTotalSize(instance) {
  return instance.getTotalSize();
}

export function getVirtualItems(instance) {
  return instance.getVirtualItems();
}

export function _didMount(instance) {
  return function () {
    return instance._didMount();
  }
}

export function _willUpdate(instance) {
  return function () {
    return instance._willUpdate();
  }
}
