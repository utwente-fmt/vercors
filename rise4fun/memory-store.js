'use strict';

function MemoryStore() {
  var hits = {};

  this.incr = function(key, cb) {
      if (hits[key]) {
          hits[key]++;
      } else {
          hits[key] = 1;
      }

      cb(null, hits[key]);
  };

  this.decrement = function(key) {
      if (hits[key]) {
          hits[key]--;
      }
  };

  // export an API to allow hits all IPs to be reset
  this.resetAll = function() {
      hits = {};
  };

  // export an API to allow hits from one IP to be reset
  this.resetKey = function(key) {
		hits[key] = 0;
      //delete hits[key];
  };
}

module.exports = MemoryStore;
